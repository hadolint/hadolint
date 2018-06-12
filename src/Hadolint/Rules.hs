{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Hadolint.Rules where

import Control.Arrow ((&&&))
import Data.List (dropWhile, isInfixOf, isPrefixOf, mapAccumL)
import Data.List.NonEmpty (toList)
import qualified Hadolint.Bash as Bash
import Language.Docker.Syntax

import Data.Semigroup ((<>))
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Void (Void)
import qualified ShellCheck.Interface
import ShellCheck.Interface (Severity(..))
import qualified Text.Megaparsec as Megaparsec
import qualified Text.Megaparsec.Char as Megaparsec

data Metadata = Metadata
    { code :: Text.Text
    , severity :: Severity
    , message :: Text.Text
    } deriving (Eq)

-- a check is the application of a rule on a specific part of code
-- the enforced result and the affected position
-- position only records the linenumber at the moment to keep it easy
-- and simple to develop new rules
-- line numbers in the negative range are meant for the global context
data RuleCheck = RuleCheck
    { metadata :: Metadata
    , filename :: Filename
    , linenumber :: Linenumber
    , success :: Bool
    } deriving (Eq)

instance Ord RuleCheck where
    a `compare` b = linenumber a `compare` linenumber b

type IgnoreRuleParser = Megaparsec.Parsec Void Text.Text

type ParsedFile = [InstructionPos Bash.ParsedBash]

link :: Metadata -> Text.Text
link (Metadata code _ _)
    | "SC" `Text.isPrefixOf` code = "https://github.com/koalaman/shellcheck/wiki/" <> code
    | "DL" `Text.isPrefixOf` code = "https://github.com/hadolint/hadolint/wiki/" <> code
    | otherwise = "https://github.com/hadolint/hadolint"

-- a Rule takes a Dockerfile with parsed bash and returns the executed checks
type Rule = ParsedFile -> [RuleCheck]

-- Apply a function on each instruction and create a check
-- for the according line number
mapInstructions ::
       Metadata
    -> (state -> Linenumber -> Instruction Bash.ParsedBash -> (state, Bool))
    -> state
    -> Rule
mapInstructions metadata f initialState dockerfile =
    let (_, results) = mapAccumL applyRule initialState dockerfile
    in results
  where
    applyRule state (InstructionPos (OnBuild i) source linenumber) =
        applyWithState state source linenumber i -- All rules applying to instructions also apply to ONBUILD,
                                                 -- so we unwrap the OnBuild constructor and check directly the inner
                                                 -- instruction
    applyRule state (InstructionPos i source linenumber) =
        applyWithState state source linenumber i -- Otherwise, normal instructions are not unwrapped
    applyWithState state source linenumber instruction =
        let (newState, res) = f state linenumber instruction
        in (newState, RuleCheck metadata source linenumber res)

instructionRule ::
       Text.Text -> Severity -> Text.Text -> (Instruction Bash.ParsedBash -> Bool) -> Rule
instructionRule code severity message check =
    instructionRuleLine code severity message (const check)

instructionRuleLine ::
       Text.Text
    -> Severity
    -> Text.Text
    -> (Linenumber -> Instruction Bash.ParsedBash -> Bool)
    -> Rule
instructionRuleLine code severity message check =
    instructionRuleState code severity message checkAndDropState ()
  where
    checkAndDropState state line instr = (state, check line instr)

instructionRuleState ::
       Text.Text
    -> Severity
    -> Text.Text
    -> (state -> Linenumber -> Instruction Bash.ParsedBash -> (state, Bool))
    -> state
    -> Rule
instructionRuleState code severity message = mapInstructions (Metadata code severity message)

withState :: a -> b -> (a, b)
withState st res = (st, res)

argumentsRule :: (Bash.ParsedBash -> a) -> Arguments Bash.ParsedBash -> a
argumentsRule applyRule args =
    case args of
        ArgumentsText as -> applyRule as
        ArgumentsList as -> applyRule as

-- Enforce rules on a dockerfile and return failed checks
analyze :: [Rule] -> Dockerfile -> [RuleCheck]
analyze list dockerfile = filter failed $ concat [r parsedFile | r <- list]
  where
    failed RuleCheck {metadata = Metadata {code}, linenumber, success} =
        not success && not (wasIgnored code linenumber)
    wasIgnored c ln = not $ null [line | (line, codes) <- allIgnores, line == ln, c `elem` codes]
    allIgnores = ignored dockerfile
    parsedFile = map (fmap Bash.parseShell) dockerfile

ignored :: Dockerfile -> [(Linenumber, [Text.Text])]
ignored dockerfile =
    [(l + 1, ignores) | (l, Just ignores) <- map (lineNumber &&& extractIgnored) dockerfile]
  where
    extractIgnored = ignoreFromInstruction . instruction
    ignoreFromInstruction (Comment comment) = parseComment comment
    ignoreFromInstruction _ = Nothing
    -- | Parses the comment text and extracts the ignored rule names
    parseComment :: Text.Text -> Maybe [Text.Text]
    parseComment = Megaparsec.parseMaybe commentParser
    commentParser :: IgnoreRuleParser [Text.Text]
    commentParser =
        spaces >> -- The parser for the ignored rules
        string "hadolint" >>
        spaces1 >>
        string "ignore=" >>
        spaces >>
        Megaparsec.sepBy1 ruleName (spaces >> string "," >> spaces)
    string = Megaparsec.string
    spaces = Megaparsec.takeWhileP Nothing space
    spaces1 = Megaparsec.takeWhile1P Nothing space
    space c = c == ' ' || c == '\t'
    ruleName = Megaparsec.takeWhile1P Nothing (\c -> c `elem` ("DLSC0123456789" :: String))

rules :: [Rule]
rules =
    [ absoluteWorkdir
    , shellcheckBash
    , invalidCmd
    , copyInsteadAdd
    , copyEndingSlash
    , copyFromExists
    , copyFromAnother
    , fromAliasUnique
    , noRootUser
    , noCd
    , noSudo
    , noAptGetUpgrade
    , noApkUpgrade
    , noLatestTag
    , noUntagged
    , aptGetVersionPinned
    , aptGetCleanup
    , apkAddVersionPinned
    , apkAddNoCache
    , useAdd
    , pipVersionPinned
    , npmVersionPinned
    , invalidPort
    , aptGetNoRecommends
    , aptGetYes
    , wgetOrCurl
    , hasNoMaintainer
    , multipleCmds
    , multipleEntrypoints
    , useShell
    ]

commentMetadata :: ShellCheck.Interface.Comment -> Metadata
commentMetadata (ShellCheck.Interface.Comment severity code message) =
    Metadata (Text.pack ("SC" ++ show code)) severity (Text.pack message)

shellcheckBash :: ParsedFile -> [RuleCheck]
shellcheckBash = concatMap check
  where
    check (InstructionPos (Run args) source linenumber) =
        argumentsRule (applyRule source linenumber) args
    check _ = []
    applyRule source linenumber args =
        rmDup [RuleCheck m source linenumber False | m <- convert args]
    convert args = [commentMetadata c | c <- Bash.shellcheck args]
    rmDup :: [RuleCheck] -> [RuleCheck]
    rmDup [] = []
    rmDup (x:xs) = x : rmDup (filter (\y -> metadata x /= metadata y) xs)

allFromImages :: ParsedFile -> [(Linenumber, BaseImage)]
allFromImages dockerfile = [(l, f) | (l, From f) <- instr]
  where
    instr = fmap (lineNumber &&& instruction) dockerfile

allAliasedImages :: ParsedFile -> [(Linenumber, ImageAlias)]
allAliasedImages dockerfile =
    [(l, alias) | (l, Just alias) <- map extractAlias (allFromImages dockerfile)]
  where
    extractAlias (l, f) = (l, fromAlias f)

allImageNames :: ParsedFile -> [(Linenumber, Text.Text)]
allImageNames dockerfile = [(l, fromName baseImage) | (l, baseImage) <- allFromImages dockerfile]

-- | Returns a list of all image aliases in FROM instructions that
--  are defined before the given line number.
previouslyDefinedAliases :: Linenumber -> ParsedFile -> [Text.Text]
previouslyDefinedAliases line dockerfile =
    [i | (l, ImageAlias i) <- allAliasedImages dockerfile, l < line]

-- | Returns the result of running the check function on the image alias
--   name, if the passed instruction is a FROM instruction with a stage alias.
--   Otherwise, returns True.
aliasMustBe :: (Text.Text -> Bool) -> Instruction a -> Bool
aliasMustBe predicate fromInstr =
    case fromInstr of
        From (UntaggedImage _ (Just (ImageAlias alias))) -> predicate alias
        From (TaggedImage _ _ (Just (ImageAlias alias))) -> predicate alias
        From (DigestedImage _ _ (Just (ImageAlias alias))) -> predicate alias
        _ -> True

fromName :: BaseImage -> Text.Text
fromName (UntaggedImage Image {imageName} _) = imageName
fromName (TaggedImage Image {imageName} _ _) = imageName
fromName (DigestedImage Image {imageName} _ _) = imageName

fromAlias :: BaseImage -> Maybe ImageAlias
fromAlias (UntaggedImage _ alias) = alias
fromAlias (TaggedImage _ _ alias) = alias
fromAlias (DigestedImage _ _ alias) = alias

absoluteWorkdir :: Rule
absoluteWorkdir = instructionRule code severity message check
  where
    code = "DL3000"
    severity = ErrorC
    message = "Use absolute WORKDIR"
    check (Workdir loc)
        | "$" `Text.isPrefixOf` loc = True
        | "/" `Text.isPrefixOf` loc = True
        | otherwise = False
    check _ = True

hasNoMaintainer :: Rule
hasNoMaintainer = instructionRule code severity message check
  where
    code = "DL4000"
    severity = ErrorC
    message = "MAINTAINER is deprecated"
    check (Maintainer _) = False
    check _ = True

-- Check if a command contains a program call in the Run instruction
usingProgram :: String -> Bash.ParsedBash -> Bool
usingProgram prog args = not $ null [cmd | cmd <- Bash.findCommandNames args, cmd == prog]

multipleCmds :: Rule
multipleCmds = instructionRuleState code severity message check Nothing
  where
    code = "DL4003"
    severity = WarningC
    message =
        "Multiple `CMD` instructions found. If you list more than one `CMD` then only the last \
        \`CMD` will take effect"
    check Nothing line (Cmd _) = withState (Just line) True -- Remember the first CMD found
    check (Just l) _ (Cmd _) = withState (Just l) False -- Fail the rule, CMD is duplicated
    check st _ _ = withState st True

multipleEntrypoints :: Rule
multipleEntrypoints = instructionRuleState code severity message check Nothing
  where
    code = "DL4004"
    severity = ErrorC
    message =
        "Multiple `ENTRYPOINT` instructions found. If you list more than one `ENTRYPOINT` then \
        \only the last `ENTRYPOINT` will take effect"
    check Nothing line (Entrypoint _) = withState (Just line) True -- Remember the first ENTRYPOINT found
    check (Just l) _ (Entrypoint _) = withState (Just l) False -- Fail the rule, ENTRYPOINT is duplicated
    check st _ _ = withState st True

wgetOrCurl :: Rule
wgetOrCurl = instructionRuleState code severity message check Set.empty
  where
    code = "DL4001"
    severity = WarningC
    message = "Either use Wget or Curl but not both"
    check state _ (Run args) = argumentsRule (detectDoubleUsage state) args
    check state _ _ = withState state True
    detectDoubleUsage state args =
        let newArgs = extractCommands args
            newState = Set.union state newArgs
        in withState newState (Set.size newState < 2)
    extractCommands args =
        Set.fromList [w | w <- Bash.findCommandNames args, w == "curl" || w == "wget"]

invalidCmd :: Rule
invalidCmd = instructionRule code severity message check
  where
    code = "DL3001"
    severity = InfoC
    message =
        "For some bash commands it makes no sense running them in a Docker container like `ssh`, \
        \`vim`, `shutdown`, `service`, `ps`, `free`, `top`, `kill`, `mount`, `ifconfig`"
    check (Run args) = argumentsRule detectInvalid args
    check _ = True
    detectInvalid args = null [arg | arg <- Bash.findCommandNames args, arg `elem` invalidCmds]
    invalidCmds = ["ssh", "vim", "shutdown", "service", "ps", "free", "top", "kill", "mount"]

noRootUser :: Rule
noRootUser = instructionRule code severity message check
  where
    code = "DL3002"
    severity = WarningC
    message = "Do not switch to root USER"
    check (User user) =
        not
            (Text.isPrefixOf "root:" user ||
             Text.isPrefixOf "0:" user || user == "root" || user == "0")
    check _ = True

noCd :: Rule
noCd = instructionRule code severity message check
  where
    code = "DL3003"
    severity = WarningC
    message = "Use WORKDIR to switch to a directory"
    check (Run args) = argumentsRule (not . usingProgram "cd") args
    check _ = True

noSudo :: Rule
noSudo = instructionRule code severity message check
  where
    code = "DL3004"
    severity = ErrorC
    message =
        "Do not use sudo as it leads to unpredictable behavior. Use a tool like gosu to enforce \
        \root"
    check (Run args) = argumentsRule (not . usingProgram "sudo") args
    check _ = True

noAptGetUpgrade :: Rule
noAptGetUpgrade = instructionRule code severity message check
  where
    code = "DL3005"
    severity = ErrorC
    message = "Do not use apt-get upgrade or dist-upgrade"
    check (Run args) = argumentsRule (Bash.noCommands (Bash.cmdHasArgs "apt-get" ["upgrade"])) args
    check _ = True

noUntagged :: Rule
noUntagged dockerfile = instructionRuleLine code severity message check dockerfile
  where
    code = "DL3006"
    severity = WarningC
    message = "Always tag the version of an image explicitly"
    check _ (From (UntaggedImage (Image _ "scratch") _)) = True
    check line (From (UntaggedImage (Image _ i) _)) =
        i `elem` previouslyDefinedAliases line dockerfile
    check _ _ = True

noLatestTag :: Rule
noLatestTag = instructionRule code severity message check
  where
    code = "DL3007"
    severity = WarningC
    message =
        "Using latest is prone to errors if the image will ever update. Pin the version explicitly \
        \to a release tag"
    check (From (TaggedImage _ tag _)) = tag /= "latest"
    check _ = True

aptGetVersionPinned :: Rule
aptGetVersionPinned = instructionRule code severity message check
  where
    code = "DL3008"
    severity = WarningC
    message =
        "Pin versions in apt get install. Instead of `apt-get install <package>` use `apt-get \
        \install <package>=<version>`"
    check (Run args) = argumentsRule (all versionFixed . aptGetPackages) args
    check _ = True
    versionFixed package = "=" `isInfixOf` package

aptGetPackages :: Bash.ParsedBash -> [String]
aptGetPackages args =
    [ arg
    | cmd <- dropTarget <$> Bash.findCommands args
    , arg <- Bash.getArgsNoFlags cmd
    , Bash.cmdHasArgs "apt-get" ["install"] cmd
    , arg /= "install"
    ]
  where
    dropTarget = Bash.dropFlagArg ["t", "target-release"]

aptGetCleanup :: Rule
aptGetCleanup dockerfile = instructionRuleState code severity message check Nothing dockerfile
  where
    code = "DL3009"
    severity = InfoC
    message = "Delete the apt-get lists after installing something"
    -- | 'check' returns a tuple (state, check_result)
    --   The state in this case is the FROM instruction where the current instruction we are
    --   inspecting is nested in.
    --   We only care for users to delete the lists folder if the FROM clase we're is is the last one
    --   or if it is used as the base image for another FROM clause.
    check _ line f@(From _) = withState (Just (line, f)) True -- Remember the last FROM instruction found
    check st@(Just (line, From baseimage)) _ (Run args) =
        withState st (argumentsRule (didNotForgetToCleanup line baseimage) args)
    check st _ _ = withState st True
    -- Check all commands in the script for the presence of apt-get update
    -- If the command is there, then we need to verify that the user is also removing the lists folder
    didNotForgetToCleanup line baseimage args
        | not (hasUpdate args) || not (imageIsUsed line baseimage) = True
        | otherwise = hasCleanup args
    hasCleanup args =
        any (Bash.cmdHasArgs "rm" ["-rf", "/var/lib/apt/lists/*"]) (Bash.findCommands args)
    hasUpdate args = any (Bash.cmdHasArgs "apt-get" ["update"]) (Bash.findCommands args)
    imageIsUsed line baseimage = isLastImage line baseimage || imageIsUsedLater line baseimage
    isLastImage line baseimage =
        case reverse (allFromImages dockerfile) of
            lst:_ -> (line, baseimage) == lst
            _ -> True
    imageIsUsedLater line baseimage =
        case fromAlias baseimage of
            Nothing -> True
            Just (ImageAlias alias) ->
                alias `elem` [i | (l, i) <- allImageNames dockerfile, l > line]

noApkUpgrade :: Rule
noApkUpgrade = instructionRule code severity message check
  where
    code = "DL3017"
    severity = ErrorC
    message = "Do not use apk upgrade"
    check (Run args) = argumentsRule (Bash.noCommands (Bash.cmdHasArgs "apk" ["upgrade"])) args
    check _ = True

apkAddVersionPinned :: Rule
apkAddVersionPinned = instructionRule code severity message check
  where
    code = "DL3018"
    severity = WarningC
    message =
        "Pin versions in apk add. Instead of `apk add <package>` use `apk add <package>=<version>`"
    check (Run args) = argumentsRule (\as -> and [versionFixed p | p <- apkAddPackages as]) args
    check _ = True
    versionFixed package = "=" `isInfixOf` package

apkAddPackages :: Bash.ParsedBash -> [String]
apkAddPackages args =
    [ arg
    | cmd <- dropTarget <$> Bash.findCommands args
    , arg <- Bash.getArgsNoFlags cmd
    , Bash.cmdHasArgs "apk" ["add"] cmd
    , arg /= "add"
    ]
  where
    dropTarget = Bash.dropFlagArg ["t", "virtual"]

apkAddNoCache :: Rule
apkAddNoCache = instructionRule code severity message check
  where
    code = "DL3019"
    severity = InfoC
    message =
        "Use the `--no-cache` switch to avoid the need to use `--update` and remove \
        \`/var/cache/apk/*` when done installing packages"
    check (Run args) = argumentsRule (Bash.noCommands forgotCacheOption) args
    check _ = True
    forgotCacheOption cmd = Bash.cmdHasArgs "apk" ["add"] cmd && not (Bash.hasFlag "no-cache" cmd)

useAdd :: Rule
useAdd = instructionRule code severity message check
  where
    code = "DL3010"
    severity = InfoC
    message = "Use ADD for extracting archives into an image"
    check (Copy (CopyArgs srcs _ _ _)) =
        and
            [ not (format `Text.isSuffixOf` src)
            | SourcePath src <- toList srcs
            , format <- archiveFormats
            ]
    check _ = True
    archiveFormats = [".tar", ".gz", ".bz2", "xz"]

invalidPort :: Rule
invalidPort = instructionRule code severity message check
  where
    code = "DL3011"
    severity = ErrorC
    message = "Valid UNIX ports range from 0 to 65535"
    check (Expose (Ports ports)) =
        and [p <= 65535 | Port p _ <- ports] &&
        and [l <= 65535 && m <= 65535 | PortRange l m _ <- ports]
    check _ = True

pipVersionPinned :: Rule
pipVersionPinned = instructionRule code severity message check
  where
    code = "DL3013"
    severity = WarningC
    message =
        "Pin versions in pip. Instead of `pip install <package>` use `pip install \
        \<package>==<version>`"
    check (Run args) = argumentsRule (Bash.noCommands forgotToPinVersion) args
    check _ = True
    forgotToPinVersion cmd = isPipInstall cmd && not (all versionFixed (packages cmd))
    -- Check if the command is a pip* install command, and that specific pacakges are being listed
    isPipInstall cmd =
        case Bash.getCommandName cmd of
            Just ('p':'i':'p':_) -> relevantInstall cmd
            _ -> False
    -- If the user is installing requirements from a file or just the local module, then we are not interested
    -- in running this rule
    relevantInstall cmd =
        ["install"] `isInfixOf` Bash.getAllArgs cmd &&
        not (["-r"] `isInfixOf` Bash.getAllArgs cmd || ["."] `isInfixOf` Bash.getAllArgs cmd)
    packages cmd = stripInstallPrefix (Bash.getArgsNoFlags cmd)
    versionFixed package = hasVersionSymbol package || isVersionedGit package
    isVersionedGit package = "git+http" `isInfixOf` package && "@" `isInfixOf` package
    versionSymbols = ["==", ">=", "<=", ">", "<", "!=", "~=", "==="]
    hasVersionSymbol package = or [s `isInfixOf` package | s <- versionSymbols]

stripInstallPrefix :: [String] -> [String]
stripInstallPrefix = dropWhile (== "install")

{-|
  Rule for pinning NPM packages to version, tag, or commit
  supported formats by Hadolint
    npm install (with no args, in package dir)
    npm install [<@scope>/]<name>
    npm install [<@scope>/]<name>@<tag>
    npm install [<@scope>/]<name>@<version>
    npm install git[+http|+https]://<git-host>/<git-user>/<repo-name>[#<commit>|#semver:<semver>]
    npm install git+ssh://<git-host>:<git-user>/<repo-name>[#<commit>|#semver:<semver>]
-}
npmVersionPinned :: Rule
npmVersionPinned = instructionRule code severity message check
  where
    code = "DL3016"
    severity = WarningC
    message =
        "Pin versions in npm. Instead of `npm install <package>` use `npm install \
        \<package>@<version>`"
    check (Run args) = argumentsRule (Bash.noCommands forgotToPinVersion) args
    check _ = True
    forgotToPinVersion cmd = isNpmInstall cmd && not (all versionFixed (packages cmd))
    isNpmInstall = Bash.cmdHasArgs "npm" ["install"]
    packages cmd = stripInstallPrefix (Bash.getArgsNoFlags cmd)
    versionFixed package =
        if hasGitPrefix package
            then isVersionedGit package
            else hasVersionSymbol package
    gitPrefixes = ["git://", "git+ssh://", "git+http://", "git+https://"]
    hasGitPrefix package = or [p `isPrefixOf` package | p <- gitPrefixes]
    isVersionedGit package = "#" `isInfixOf` package
    hasVersionSymbol package = "@" `isInfixOf` dropScope package
      where
        dropScope pkg =
            if "@" `isPrefixOf` pkg
                then dropWhile ('/' <) pkg
                else pkg

aptGetYes :: Rule
aptGetYes = instructionRule code severity message check
  where
    code = "DL3014"
    severity = WarningC
    message = "Use the `-y` switch to avoid manual input `apt-get -y install <package>`"
    check (Run args) = argumentsRule (Bash.noCommands forgotAptYesOption) args
    check _ = True
    forgotAptYesOption cmd = isAptGetInstall cmd && not (hasYesOption cmd)
    isAptGetInstall = Bash.cmdHasArgs "apt-get" ["install"]
    hasYesOption cmd =
        "y" `elem` allFlags cmd ||
        "yes" `elem` allFlags cmd || length (filter (== "q") (allFlags cmd)) > 1
    allFlags cmd = snd <$> Bash.getAllFlags cmd

aptGetNoRecommends :: Rule
aptGetNoRecommends = instructionRule code severity message check
  where
    code = "DL3015"
    severity = InfoC
    message = "Avoid additional packages by specifying `--no-install-recommends`"
    check (Run args) = argumentsRule (Bash.noCommands forgotNoInstallRecommends) args
    check _ = True
    forgotNoInstallRecommends cmd = isAptGetInstall cmd && not (hasRecommendsOption cmd)
    isAptGetInstall = Bash.cmdHasArgs "apt-get" ["install"]
    hasRecommendsOption = Bash.hasFlag "no-install-recommends"

isArchive :: Text.Text -> Bool
isArchive path =
    True `elem`
    [ ftype `Text.isSuffixOf` path
    | ftype <-
          [ ".tar"
          , ".gz"
          , ".bz2"
          , ".xz"
          , ".zip"
          , ".tgz"
          , ".tb2"
          , ".tbz"
          , ".tbz2"
          , ".lz"
          , ".lzma"
          , ".tlz"
          , ".txz"
          , ".Z"
          , ".tZ"
          ]
    ]

isUrl :: Text.Text -> Bool
isUrl path = True `elem` [proto `Text.isPrefixOf` path | proto <- ["https://", "http://"]]

copyInsteadAdd :: Rule
copyInsteadAdd = instructionRule code severity message check
  where
    code = "DL3020"
    severity = ErrorC
    message = "Use COPY instead of ADD for files and folders"
    check (Add (AddArgs srcs _ _)) =
        and [isArchive src || isUrl src | SourcePath src <- toList srcs]
    check _ = True

copyEndingSlash :: Rule
copyEndingSlash = instructionRule code severity message check
  where
    code = "DL3021"
    severity = ErrorC
    message = "COPY with more than 2 arguments requires the last argument to end with /"
    check (Copy (CopyArgs sources t _ _))
        | length sources > 1 = endsWithSlash t
        | otherwise = True
    check _ = True
    endsWithSlash (TargetPath t) = Text.last t == '/' -- it is safe to use last, as the target is never empty

copyFromExists :: Rule
copyFromExists dockerfile = instructionRuleLine code severity message check dockerfile
  where
    code = "DL3022"
    severity = WarningC
    message = "COPY --from should reference a previously defined FROM alias"
    check l (Copy (CopyArgs _ _ _ (CopySource s))) = s `elem` previouslyDefinedAliases l dockerfile
    check _ _ = True

copyFromAnother :: Rule
copyFromAnother = instructionRuleState code severity message check Nothing
  where
    code = "DL3023"
    severity = ErrorC
    message = "COPY --from should reference a previously defined FROM alias"
    -- | 'check' returns a tuple (state, check_result)
    --   The state in this case is the FROM instruction where the current instruction we are
    --   inspecting is nested in.
    check _ _ f@(From _) = withState (Just f) True -- Remember the last FROM instruction found
    check st@(Just fromInstr) _ (Copy (CopyArgs _ _ _ (CopySource stageName))) =
        withState st (aliasMustBe (/= stageName) fromInstr) -- Cannot copy from itself!
    check state _ _ = withState state True

fromAliasUnique :: Rule
fromAliasUnique dockerfile = instructionRuleLine code severity message check dockerfile
  where
    code = "DL3024"
    severity = ErrorC
    message = "FROM aliases (stage names) must be unique"
    check line = aliasMustBe (not . alreadyTaken line)
    alreadyTaken line alias = alias `elem` previouslyDefinedAliases line dockerfile

useShell :: Rule
useShell = instructionRule code severity message check
  where
    code = "DL4005"
    severity = WarningC
    message = "Use SHELL to change the default shell"
    check (Run args) = argumentsRule (Bash.noCommands (Bash.cmdHasArgs "ln" ["/bin/sh"])) args
    check _ = True
