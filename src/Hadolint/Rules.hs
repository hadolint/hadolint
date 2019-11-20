{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Hadolint.Rules where

import Control.Arrow ((&&&))
import Data.List (dropWhile, foldl', isInfixOf, isPrefixOf, mapAccumL, nub)
import Data.List.NonEmpty (toList)
import qualified Hadolint.Shell as Shell
import Language.Docker.Syntax

import qualified Data.Map as Map
import Data.Semigroup (Semigroup, (<>))
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

-- | Contains the required parameters for optional rules
newtype RulesConfig = RulesConfig
    { allowedRegistries :: Set.Set Registry -- ^ The docker registries that are allowed in FROM
    } deriving (Show, Eq)

instance Ord RuleCheck where
    a `compare` b = linenumber a `compare` linenumber b

instance Semigroup RulesConfig where
    RulesConfig a <> RulesConfig b = RulesConfig (a <> b)

instance Monoid RulesConfig where
    mempty = RulesConfig mempty
    mappend = (<>)

type IgnoreRuleParser = Megaparsec.Parsec Void Text.Text

type ParsedFile = [InstructionPos Shell.ParsedShell]

-- | A function to check individual dockerfile instructions.
-- It gets the current state and a line number.
-- It should return the new state and whether or not the check passes for the given instruction.
type SimpleCheckerWithState state
     = state -> Linenumber -> Instruction Shell.ParsedShell -> (state, Bool)

-- | A function to check individual dockerfile instructions.
-- It gets the current line number.
-- It should return True if the check passes for the given instruction.
type SimpleCheckerWithLine = (Linenumber -> Instruction Shell.ParsedShell -> Bool)

-- | A function to check individual dockerfile instructions.
-- It should return the new state and a list of Metadata records.
-- Each Metadata record signifies a failing check for the given instruction.
type CheckerWithState state
     = state -> Linenumber -> Instruction Shell.ParsedShell -> (state, [Metadata])

link :: Metadata -> Text.Text
link (Metadata code _ _)
    | "SC" `Text.isPrefixOf` code = "https://github.com/koalaman/shellcheck/wiki/" <> code
    | "DL" `Text.isPrefixOf` code = "https://github.com/hadolint/hadolint/wiki/" <> code
    | otherwise = "https://github.com/hadolint/hadolint"

-- a Rule takes a Dockerfile with parsed shell and returns the executed checks
type Rule = ParsedFile -> [RuleCheck]

-- Apply a function on each instruction and create a check
-- for the according line number
mapInstructions :: CheckerWithState state -> state -> Rule
mapInstructions f initialState dockerfile =
    let (_, results) = mapAccumL applyRule initialState dockerfile
     in concat results
  where
    applyRule state (InstructionPos (OnBuild i) source linenumber) =
        applyWithState state source linenumber i -- All rules applying to instructions also apply to ONBUILD,
                                                 -- so we unwrap the OnBuild constructor and check directly the inner
                                                 -- instruction
    applyRule state (InstructionPos i source linenumber) =
        applyWithState state source linenumber i -- Otherwise, normal instructions are not unwrapped
    applyWithState state source linenumber instruction =
        let (newState, res) = f state linenumber instruction
         in (newState, [RuleCheck m source linenumber False | m <- res])

instructionRule ::
       Text.Text -> Severity -> Text.Text -> (Instruction Shell.ParsedShell -> Bool) -> Rule
instructionRule code severity message check =
    instructionRuleLine code severity message (const check)

instructionRuleLine :: Text.Text -> Severity -> Text.Text -> SimpleCheckerWithLine -> Rule
instructionRuleLine code severity message check =
    instructionRuleState code severity message checkAndDropState ()
  where
    checkAndDropState state line instr = (state, check line instr)

instructionRuleState ::
       Text.Text -> Severity -> Text.Text -> SimpleCheckerWithState state -> state -> Rule
instructionRuleState code severity message f = mapInstructions constMetadataCheck
  where
    meta = Metadata code severity message
    constMetadataCheck st ln instr =
        let (newSt, success) = f st ln instr
         in if not success
                then (newSt, [meta])
                else (newSt, [])

withState :: a -> b -> (a, b)
withState st res = (st, res)

argumentsRule :: (Shell.ParsedShell -> a) -> Arguments Shell.ParsedShell -> a
argumentsRule applyRule args =
    case args of
        ArgumentsText as -> applyRule as
        ArgumentsList as -> applyRule as

-- Enforce rules on a dockerfile and return failed checks
analyze :: [Rule] -> Dockerfile -> [RuleCheck]
analyze list dockerfile =
    [ result -- Keep the result
    | rule <- list -- for each rule in the list
    , result <- rule parsedFile -- after applying the rule to the file
    , notIgnored result -- and only keep failures that were not ignored
    ]
  where
    notIgnored RuleCheck {metadata = Metadata {code}, linenumber} = not (wasIgnored code linenumber)
    wasIgnored c ln = not $ null [line | (line, codes) <- allIgnores, line == ln, c `elem` codes]
    allIgnores = ignored dockerfile
    parsedFile = map (fmap Shell.parseShell) dockerfile

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
    , shellcheck
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
    , useJsonArgs
    , usePipefail
    , noApt
    , gemVersionPinned
    ]

optionalRules :: RulesConfig -> [Rule]
optionalRules RulesConfig {allowedRegistries} = [registryIsAllowed allowedRegistries]

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
        From BaseImage {alias = Just (ImageAlias as)} -> predicate as
        _ -> True

fromName :: BaseImage -> Text.Text
fromName BaseImage {image = Image {imageName}} = imageName

fromAlias :: BaseImage -> Maybe ImageAlias
fromAlias BaseImage {alias} = alias

-------------
--  RULES  --
-------------
shellcheck :: Rule
shellcheck = mapInstructions check Shell.defaultShellOpts
  where
    check :: CheckerWithState Shell.ShellOpts
    check _ _ (From _) = (Shell.defaultShellOpts, []) -- Reset the state
    check st _ (Arg name _) = (Shell.addVars [name] st, [])
    check st _ (Env pairs) = (Shell.addVars (map fst pairs) st, [])
    check st _ (Shell (ArgumentsList script)) = (Shell.setShell (Shell.original script) st, [])
    check st _ (Shell (ArgumentsText script)) = (Shell.setShell (Shell.original script) st, [])
    check st _ (Run (ArgumentsList script)) = (st, doCheck st script)
    check st _ (Run (ArgumentsText script)) = (st, doCheck st script)
    check st _ _ = (st, [])
    doCheck opts script = nub [commentMetadata c | c <- Shell.shellcheck opts script]

-- | Converts ShellCheck errors into our own errors type
commentMetadata :: ShellCheck.Interface.PositionedComment -> Metadata
commentMetadata c =
    Metadata (Text.pack ("SC" ++ show (code c))) (severity c) (Text.pack (message c))
  where
    severity pc = ShellCheck.Interface.cSeverity $ ShellCheck.Interface.pcComment pc
    code pc = ShellCheck.Interface.cCode $ ShellCheck.Interface.pcComment pc
    message pc = ShellCheck.Interface.cMessage $ ShellCheck.Interface.pcComment pc

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
usingProgram :: Text.Text -> Shell.ParsedShell -> Bool
usingProgram prog args = not $ null [cmd | cmd <- Shell.findCommandNames args, cmd == prog]

multipleCmds :: Rule
multipleCmds = instructionRuleState code severity message check False
  where
    code = "DL4003"
    severity = WarningC
    message =
        "Multiple `CMD` instructions found. If you list more than one `CMD` then only the last \
        \`CMD` will take effect"
    check _ _ From {} = withState False True -- Reset the state each time we find a FROM
    check st _ Cmd {} = withState True (not st) -- Remember we found a CMD, fail if we found a CMD before
    check st _ _ = withState st True

multipleEntrypoints :: Rule
multipleEntrypoints = instructionRuleState code severity message check False
  where
    code = "DL4004"
    severity = ErrorC
    message =
        "Multiple `ENTRYPOINT` instructions found. If you list more than one `ENTRYPOINT` then \
        \only the last `ENTRYPOINT` will take effect"
    check _ _ From {} = withState False True -- Reset the state each time we find a FROM
    check st _ Entrypoint {} = withState True (not st) -- Remember we found an ENTRYPOINT
                                                       -- and fail if we found another one before
    check st _ _ = withState st True

wgetOrCurl :: Rule
wgetOrCurl = instructionRuleState code severity message check Set.empty
  where
    code = "DL4001"
    severity = WarningC
    message = "Either use Wget or Curl but not both"
    check state _ (Run args) = argumentsRule (detectDoubleUsage state) args
    check _ _ (From _) = withState Set.empty True -- Reset the state for each stage
    check state _ _ = withState state True
    detectDoubleUsage state args =
        let newArgs = extractCommands args
            newState = Set.union state newArgs
         in withState newState (Set.null newArgs || Set.size newState < 2)
    extractCommands args =
        Set.fromList [w | w <- Shell.findCommandNames args, w == "curl" || w == "wget"]

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
    detectInvalid args = null [arg | arg <- Shell.findCommandNames args, arg `elem` invalidCmds]
    invalidCmds = ["ssh", "vim", "shutdown", "service", "ps", "free", "top", "kill", "mount"]

noRootUser :: Rule
noRootUser dockerfile = instructionRuleState code severity message check Nothing dockerfile
  where
    code = "DL3002"
    severity = WarningC
    message = "Last USER should not be root"
    check _ _ (From from) = withState (Just from) True -- Remember the last FROM instruction found
    check st@(Just from) line (User user)
        | isRoot user && lastUserIsRoot from line = withState st False
        | otherwise = withState st True
    check st _ _ = withState st True
    --
    --
    lastUserIsRoot from line = Map.lookup from rootStages == Just line
    --
    --
    rootStages :: Map.Map BaseImage Linenumber
    rootStages =
        let indexedInstructions = map (instruction &&& lineNumber) dockerfile
            (_, usersMap) = foldl' buildMap (Nothing, Map.empty) indexedInstructions
         in usersMap
    --
    --
    buildMap (_, st) (From from, _) = (Just from, st) -- Remember the FROM we are currently inspecting
    buildMap (Just from, st) (User user, line)
        | isRoot user = (Just from, Map.insert from line st) -- Remember the line with a root user
        | otherwise = (Just from, Map.delete from st) -- Forget there was a root used for this FROM
    buildMap st _ = st
    --
    --
    isRoot user =
        Text.isPrefixOf "root:" user || Text.isPrefixOf "0:" user || user == "root" || user == "0"

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
    check (Run args) =
        argumentsRule (Shell.noCommands (Shell.cmdHasArgs "apt-get" ["upgrade"])) args
    check _ = True

noUntagged :: Rule
noUntagged dockerfile = instructionRuleLine code severity message check dockerfile
  where
    code = "DL3006"
    severity = WarningC
    message = "Always tag the version of an image explicitly"
    check _ (From BaseImage {image = (Image _ "scratch")}) = True
    check _ (From BaseImage {digest = Just _}) = True
    check line (From BaseImage {image = (Image _ i), tag = Nothing}) =
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
    check (From BaseImage {tag = Just t}) = t /= "latest"
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
    versionFixed package = "=" `Text.isInfixOf` package

aptGetPackages :: Shell.ParsedShell -> [Text.Text]
aptGetPackages args =
    [ arg
    | cmd <- Shell.presentCommands args
    , Shell.cmdHasArgs "apt-get" ["install"] cmd
    , arg <- Shell.getArgsNoFlags (dropTarget cmd)
    , arg /= "install"
    ]
  where
    dropTarget = Shell.dropFlagArg ["t", "target-release"]

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
        any (Shell.cmdHasArgs "rm" ["-rf", "/var/lib/apt/lists/*"]) (Shell.presentCommands args)
    hasUpdate args = any (Shell.cmdHasArgs "apt-get" ["update"]) (Shell.presentCommands args)
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
    check (Run args) = argumentsRule (Shell.noCommands (Shell.cmdHasArgs "apk" ["upgrade"])) args
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
    versionFixed package = "=" `Text.isInfixOf` package

apkAddPackages :: Shell.ParsedShell -> [Text.Text]
apkAddPackages args =
    [ arg
    | cmd <- Shell.presentCommands args
    , Shell.cmdHasArgs "apk" ["add"] cmd
    , arg <- Shell.getArgsNoFlags (dropTarget cmd)
    , arg /= "add"
    ]
  where
    dropTarget = Shell.dropFlagArg ["t", "virtual", "repository"]

apkAddNoCache :: Rule
apkAddNoCache = instructionRule code severity message check
  where
    code = "DL3019"
    severity = InfoC
    message =
        "Use the `--no-cache` switch to avoid the need to use `--update` and remove \
        \`/var/cache/apk/*` when done installing packages"
    check (Run args) = argumentsRule (Shell.noCommands forgotCacheOption) args
    check _ = True
    forgotCacheOption cmd = Shell.cmdHasArgs "apk" ["add"] cmd && not (Shell.hasFlag "no-cache" cmd)

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
    archiveFormats =
        [ ".tar"
        , ".tar.bz2"
        , ".tb2"
        , ".tbz"
        , ".tbz2"
        , ".tar.gz"
        , ".tgz"
        , ".tpz"
        , ".tar.lz"
        , ".tar.lzma"
        , ".tlz"
        , ".tar.xz"
        , ".txz"
        , ".tar.Z"
        , ".tZ"
        ]

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
    check (Run args) = argumentsRule (Shell.noCommands forgotToPinVersion) args
    check _ = True
    forgotToPinVersion cmd =
        isPipInstall cmd && not (hasBuildConstraint cmd) && not (all versionFixed (packages cmd))
    -- Check if the command is a pip* install command, and that specific pacakges are being listed
    isPipInstall cmd@(Shell.Command name _ _) = "pip" `Text.isPrefixOf` name && relevantInstall cmd
    -- If the user is installing requirements from a file or just the local module, then we are not interested
    -- in running this rule
    relevantInstall cmd =
        ["install"] `isInfixOf` Shell.getArgs cmd &&
        not (["--requirement"] `isInfixOf` Shell.getArgs cmd || ["-r"] `isInfixOf` Shell.getArgs cmd || ["."] `isInfixOf` Shell.getArgs cmd)
    hasBuildConstraint = Shell.hasFlag "constraint"
    packages cmd =
        stripInstallPrefix $
        Shell.getArgsNoFlags $ Shell.dropFlagArg ["i", "index-url", "extra-index-url"] cmd
    versionFixed package = hasVersionSymbol package || isVersionedGit package
    isVersionedGit package = "git+http" `Text.isInfixOf` package && "@" `Text.isInfixOf` package
    versionSymbols = ["==", ">=", "<=", ">", "<", "!=", "~=", "==="]
    hasVersionSymbol package = or [s `Text.isInfixOf` package | s <- versionSymbols]

stripInstallPrefix :: [Text.Text] -> [Text.Text]
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
    check (Run args) = argumentsRule (Shell.noCommands forgotToPinVersion) args
    check _ = True
    forgotToPinVersion cmd =
        isNpmInstall cmd && installIsFirst cmd && not (all versionFixed (packages cmd))
    isNpmInstall = Shell.cmdHasArgs "npm" ["install"]
    installIsFirst cmd = ["install"] `isPrefixOf` Shell.getArgsNoFlags cmd
    packages cmd = stripInstallPrefix (Shell.getArgsNoFlags cmd)
    versionFixed package =
        if hasGitPrefix package
            then isVersionedGit package
            else hasVersionSymbol package
    gitPrefixes = ["git://", "git+ssh://", "git+http://", "git+https://"]
    hasGitPrefix package = or [p `Text.isPrefixOf` package | p <- gitPrefixes]
    isVersionedGit package = "#" `Text.isInfixOf` package
    hasVersionSymbol package = "@" `Text.isInfixOf` dropScope package
      where
        dropScope pkg =
            if "@" `Text.isPrefixOf` pkg
                then Text.dropWhile ('/' <) pkg
                else pkg

aptGetYes :: Rule
aptGetYes = instructionRule code severity message check
  where
    code = "DL3014"
    severity = WarningC
    message = "Use the `-y` switch to avoid manual input `apt-get -y install <package>`"
    check (Run args) = argumentsRule (Shell.noCommands forgotAptYesOption) args
    check _ = True
    forgotAptYesOption cmd = isAptGetInstall cmd && not (hasYesOption cmd)
    isAptGetInstall = Shell.cmdHasArgs "apt-get" ["install"]
    hasYesOption = Shell.hasAnyFlag ["y", "yes", "q", "assume-yes"]

aptGetNoRecommends :: Rule
aptGetNoRecommends = instructionRule code severity message check
  where
    code = "DL3015"
    severity = InfoC
    message = "Avoid additional packages by specifying `--no-install-recommends`"
    check (Run args) = argumentsRule (Shell.noCommands forgotNoInstallRecommends) args
    check _ = True
    forgotNoInstallRecommends cmd = isAptGetInstall cmd && not (disablesRecommendOption cmd)
    isAptGetInstall = Shell.cmdHasArgs "apt-get" ["install"]
    disablesRecommendOption cmd =
        Shell.hasFlag "no-install-recommends" cmd ||
        Shell.hasArg "APT::Install-Recommends=false" cmd

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
    check (Run args) = argumentsRule (Shell.noCommands (Shell.cmdHasArgs "ln" ["/bin/sh"])) args
    check _ = True

useJsonArgs :: Rule
useJsonArgs = instructionRule code severity message check
  where
    code = "DL3025"
    severity = WarningC
    message = "Use arguments JSON notation for CMD and ENTRYPOINT arguments"
    check (Cmd (ArgumentsText _)) = False
    check (Entrypoint (ArgumentsText _)) = False
    check _ = True

noApt :: Rule
noApt = instructionRule code severity message check
  where
    code = "DL3027"
    severity = WarningC
    message =
        "Do not use apt as it is meant to be a end-user tool, use apt-get or apt-cache instead"
    check (Run args) = argumentsRule (not . usingProgram "apt") args
    check _ = True

usePipefail :: Rule
usePipefail = instructionRuleState code severity message check False
  where
    code = "DL4006"
    severity = WarningC
    message = "Set the SHELL option -o pipefail before RUN with a pipe in it. If you are using \
              \/bin/sh in an alpine image or if your shell is symlinked to busybox then consider \
              \explicitly setting your SHELL to /bin/ash, or disable this check"
    check _ _ From {} = (False, True) -- Reset the state each time we find a new FROM
    check _ _ (Shell args)
        | argumentsRule isPowerShell args = (True, True)
        | otherwise = (argumentsRule hasPipefailOption args, True)
    check False _ (Run args) = (False, argumentsRule notHasPipes args)
    check st _ _ = (st, True)
    isPowerShell (Shell.ParsedShell orig _ _) = "pwsh" `Text.isPrefixOf` orig
    notHasPipes script = not (Shell.hasPipes script)
    hasPipefailOption script =
        not $
        null
            [ True
            | cmd@(Shell.Command name arguments _) <- Shell.presentCommands script
            , validShell <- ["/bin/bash", "/bin/zsh", "/bin/ash", "bash", "zsh", "ash"]
            , name == validShell
            , Shell.hasFlag "o" cmd
            , arg <- Shell.arg <$> arguments
            , arg == "pipefail"
            ]

registryIsAllowed :: Set.Set Registry -> Rule
registryIsAllowed allowed = instructionRuleState code severity message check Set.empty
  where
    code = "DL3026"
    severity = ErrorC
    message = "Use only an allowed registry in the FROM image"
    check st _ (From BaseImage {image, alias}) = withState (Set.insert alias st) (doCheck st image)
    check st _ _ = (st, True)
    -- |Transforms an Image into a Maybe ImageAlias by using the Image name
    toImageAlias = Just . ImageAlias . imageName
    -- | Returns True if the image being used is a previous aliased image
    -- or if the image registry is in the set of allowed registries
    doCheck st img = Set.member (toImageAlias img) st || Set.null allowed || isAllowed img
    isAllowed Image {registryName = Just registry} = Set.member registry allowed
    isAllowed Image {registryName = Nothing, imageName} =
        imageName == "scratch" ||
        Set.member "docker.io" allowed || Set.member "hub.docker.com" allowed

gemVersionPinned :: Rule
gemVersionPinned = instructionRule code severity message check
  where
    code = "DL3028"
    severity = WarningC
    message =
        "Pin versions in gem install. Instead of `gem install <gem>` use `gem \
        \install <gem>:<version>`"
    check (Run args) = argumentsRule (all versionFixed . gems) args
    check _ = True
    versionFixed package = ":" `Text.isInfixOf` package

gems :: Shell.ParsedShell -> [Text.Text]
gems shell =
    [ arg
    | cmd <- Shell.presentCommands shell
    , Shell.cmdHasArgs "gem" ["install", "i"] cmd
    , not (Shell.cmdHasArgs "gem" ["-v"] cmd)
    , not (Shell.cmdHasArgs "gem" ["--version"] cmd)
    , not (Shell.cmdHasPrefixArg "gem" "--version=" cmd)
    , arg <- Shell.getArgsNoFlags cmd
    , arg /= "install"
    , arg /= "i"
    , arg /= "--"
    ]
