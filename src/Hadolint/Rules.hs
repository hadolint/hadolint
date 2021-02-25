{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Hadolint.Rules where

import Control.Arrow ((&&&))
import Control.DeepSeq (NFData)
import Data.List (foldl', isInfixOf, isPrefixOf, mapAccumL, nub)
import Data.List.NonEmpty (toList)
import Data.List.Index
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Void (Void)
import GHC.Generics (Generic)
import qualified Hadolint.Shell as Shell
import Language.Docker.Syntax
import ShellCheck.Interface (Severity (..))
import qualified ShellCheck.Interface
import qualified Text.Megaparsec as Megaparsec
import qualified Text.Megaparsec.Char as Megaparsec

data DLSeverity
  = DLErrorC
  | DLWarningC
  | DLInfoC
  | DLStyleC
  | DLIgnoreC
  deriving (Show, Eq, Ord, Generic, NFData)

data Metadata = Metadata
  { code :: Text.Text,
    severity :: DLSeverity,
    message :: Text.Text
  }
  deriving (Eq, Show)

-- a check is the application of a rule on a specific part of code
-- the enforced result and the affected position
-- position only records the linenumber at the moment to keep it easy
-- and simple to develop new rules
-- line numbers in the negative range are meant for the global context
data RuleCheck = RuleCheck
  { metadata :: Metadata,
    filename :: Filename,
    linenumber :: Linenumber,
    success :: Bool
  }
  deriving (Eq, Show)

-- | Contains the required parameters for optional rules
newtype RulesConfig = RulesConfig
  { -- | The docker registries that are allowed in FROM
    allowedRegistries :: Set.Set Registry
  }
  deriving (Show, Eq)

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
type SimpleCheckerWithState state =
  state -> Linenumber -> Instruction Shell.ParsedShell -> (state, Bool)

-- | A function to check individual dockerfile instructions.
-- It gets the current line number.
-- It should return True if the check passes for the given instruction.
type SimpleCheckerWithLine = (Linenumber -> Instruction Shell.ParsedShell -> Bool)

-- | A function to check individual dockerfile instructions.
-- It should return the new state and a list of Metadata records.
-- Each Metadata record signifies a failing check for the given instruction.
type CheckerWithState state =
  state -> Linenumber -> Instruction Shell.ParsedShell -> (state, [Metadata])

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
    applyRule state (InstructionPos onbuild@(OnBuild i) source linenumber) =
      -- All rules applying to instructions also apply to ONBUILD,
      -- so we unwrap the OnBuild constructor and check directly the inner
      -- instruction. Then we also check the instruction itself and append the
      -- result to avoid losing out on detecting problems with `ONBUILD`
      let (innerState, innerResults) = applyWithState state source linenumber i
          (finalState, outerResults) = applyWithState innerState source linenumber onbuild
       in (finalState, innerResults <> outerResults)
    applyRule state (InstructionPos i source linenumber) =
      applyWithState state source linenumber i -- Otherwise, normal instructions are not unwrapped
    applyWithState state source linenumber instruction =
      let (newState, res) = f state linenumber instruction
       in (newState, [RuleCheck m source linenumber False | m <- res])

instructionRule ::
  Text.Text -> DLSeverity -> Text.Text -> (Instruction Shell.ParsedShell -> Bool) -> Rule
instructionRule code severity message check =
  instructionRuleLine code severity message (const check)

instructionRuleLine :: Text.Text -> DLSeverity -> Text.Text -> SimpleCheckerWithLine -> Rule
instructionRuleLine code severity message check =
  instructionRuleState code severity message checkAndDropState ()
  where
    checkAndDropState state line instr = (state, check line instr)

instructionRuleState ::
  Text.Text -> DLSeverity -> Text.Text -> SimpleCheckerWithState state -> state -> Rule
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
    | rule <- list, -- for each rule in the list
      result <- rule parsedFile, -- after applying the rule to the file
      notIgnored result -- and only keep failures that were not ignored
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
    parseComment :: Text.Text -> Maybe [Text.Text]
    parseComment = Megaparsec.parseMaybe commentParser
    commentParser :: IgnoreRuleParser [Text.Text]
    commentParser =
      spaces
        >> string "hadolint" -- The parser for the ignored rules
        >> spaces1
        >> string "ignore="
        >> spaces
        >> Megaparsec.sepBy1 ruleName (spaces >> string "," >> spaces)
    string = Megaparsec.string
    spaces = Megaparsec.takeWhileP Nothing space
    spaces1 = Megaparsec.takeWhile1P Nothing space
    space c = c == ' ' || c == '\t'
    ruleName = Megaparsec.takeWhile1P Nothing (\c -> c `elem` ("DLSC0123456789" :: String))

rules :: [Rule]
rules =
  [ absoluteWorkdir,
    shellcheck,
    invalidCmd,
    copyInsteadAdd,
    copyEndingSlash,
    copyFromExists,
    copyFromAnother,
    fromAliasUnique,
    noRootUser,
    noCd,
    noSudo,
    noAptGetUpgrade,
    noApkUpgrade,
    noLatestTag,
    noUntagged,
    noPlatformFlag,
    aptGetVersionPinned,
    aptGetCleanup,
    apkAddVersionPinned,
    apkAddNoCache,
    useAdd,
    pipVersionPinned,
    npmVersionPinned,
    invalidPort,
    aptGetNoRecommends,
    aptGetYes,
    wgetOrCurl,
    hasNoMaintainer,
    multipleCmds,
    multipleEntrypoints,
    useShell,
    useJsonArgs,
    usePipefail,
    noApt,
    gemVersionPinned,
    yumYes,
    noYumUpdate,
    yumCleanup,
    yumVersionPinned,
    zypperYes,
    noZypperUpdate,
    zypperCleanup,
    zypperVersionPinned,
    dnfYes,
    noDnfUpdate,
    dnfCleanup,
    dnfVersionPinned,
    pipNoCacheDir,
    noIllegalInstructionInOnbuild,
    noSelfreferencingEnv
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
    check st _ (Run (RunArgs (ArgumentsList script) _)) = (st, doCheck st script)
    check st _ (Run (RunArgs (ArgumentsText script) _)) = (st, doCheck st script)
    check st _ _ = (st, [])
    doCheck opts script = nub [commentMetadata c | c <- Shell.shellcheck opts script]

-- | Converts ShellCheck errors into our own errors type
commentMetadata :: ShellCheck.Interface.PositionedComment -> Metadata
commentMetadata c =
  Metadata (Text.pack ("SC" ++ show (code c))) (getDLSeverity $ severity c) (Text.pack (message c))
  where
    getDLSeverity :: Severity -> DLSeverity
    getDLSeverity s =
      case s of
        WarningC -> DLWarningC
        InfoC -> DLInfoC
        StyleC -> DLStyleC
        _ -> DLErrorC
    severity pc = ShellCheck.Interface.cSeverity $ ShellCheck.Interface.pcComment pc
    code pc = ShellCheck.Interface.cCode $ ShellCheck.Interface.pcComment pc
    message pc = ShellCheck.Interface.cMessage $ ShellCheck.Interface.pcComment pc

absoluteWorkdir :: Rule
absoluteWorkdir = instructionRule code severity message check
  where
    code = "DL3000"
    severity = DLErrorC
    message = "Use absolute WORKDIR"
    check (Workdir loc)
      | "$" `Text.isPrefixOf` Text.dropAround dropQuotes loc = True
      | "/" `Text.isPrefixOf` Text.dropAround dropQuotes loc = True
      | otherwise = False
    check _ = True
    dropQuotes chr
      | chr == '\"' = True
      | chr == '\'' = True
      | otherwise = False

hasNoMaintainer :: Rule
hasNoMaintainer = instructionRule code severity message check
  where
    code = "DL4000"
    severity = DLErrorC
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
    severity = DLWarningC
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
    severity = DLErrorC
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
    severity = DLWarningC
    message = "Either use Wget or Curl but not both"
    check state _ (Run (RunArgs args _)) = argumentsRule (detectDoubleUsage state) args
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
    severity = DLInfoC
    message =
      "For some bash commands it makes no sense running them in a Docker container like `ssh`, \
      \`vim`, `shutdown`, `service`, `ps`, `free`, `top`, `kill`, `mount`, `ifconfig`"
    check (Run (RunArgs args _)) = argumentsRule detectInvalid args
    check _ = True
    detectInvalid args = null [arg | arg <- Shell.findCommandNames args, arg `elem` invalidCmds]
    invalidCmds = ["ssh", "vim", "shutdown", "service", "ps", "free", "top", "kill", "mount"]

noRootUser :: Rule
noRootUser dockerfile = instructionRuleState code severity message check Nothing dockerfile
  where
    code = "DL3002"
    severity = DLWarningC
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
    severity = DLWarningC
    message = "Use WORKDIR to switch to a directory"
    check (Run (RunArgs args _)) = argumentsRule (not . usingProgram "cd") args
    check _ = True

noSudo :: Rule
noSudo = instructionRule code severity message check
  where
    code = "DL3004"
    severity = DLErrorC
    message =
      "Do not use sudo as it leads to unpredictable behavior. Use a tool like gosu to enforce \
      \root"
    check (Run (RunArgs args _)) = argumentsRule (not . usingProgram "sudo") args
    check _ = True

noAptGetUpgrade :: Rule
noAptGetUpgrade = instructionRule code severity message check
  where
    code = "DL3005"
    severity = DLErrorC
    message = "Do not use apt-get upgrade or dist-upgrade"
    check (Run (RunArgs args _)) =
      argumentsRule (Shell.noCommands (Shell.cmdHasArgs "apt-get" ["upgrade", "dist-upgrade"])) args
    check _ = True

noUntagged :: Rule
noUntagged dockerfile = instructionRuleLine code severity message check dockerfile
  where
    code = "DL3006"
    severity = DLWarningC
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
    severity = DLWarningC
    message =
      "Using latest is prone to errors if the image will ever update. Pin the version explicitly \
      \to a release tag"
    check (From BaseImage {tag = Just t}) = t /= "latest"
    check _ = True

aptGetVersionPinned :: Rule
aptGetVersionPinned = instructionRule code severity message check
  where
    code = "DL3008"
    severity = DLWarningC
    message =
      "Pin versions in apt get install. Instead of `apt-get install <package>` use `apt-get \
      \install <package>=<version>`"
    check (Run (RunArgs args _)) = argumentsRule (all versionFixed . aptGetPackages) args
    check _ = True
    versionFixed package = "=" `Text.isInfixOf` package || ("/" `Text.isInfixOf` package || ".deb" `Text.isSuffixOf` package)

aptGetPackages :: Shell.ParsedShell -> [Text.Text]
aptGetPackages args =
  [ arg
    | cmd <- Shell.presentCommands args,
      Shell.cmdHasArgs "apt-get" ["install"] cmd,
      arg <- Shell.getArgsNoFlags (dropTarget cmd),
      arg /= "install"
  ]
  where
    dropTarget = Shell.dropFlagArg ["t", "target-release"]

aptGetCleanup :: Rule
aptGetCleanup dockerfile = instructionRuleState code severity message check Nothing dockerfile
  where
    code = "DL3009"
    severity = DLInfoC
    message = "Delete the apt-get lists after installing something"

    check _ line f@(From _) = withState (Just (line, f)) True -- Remember the last FROM instruction found
    check st@(Just (line, From baseimage)) _ (Run (RunArgs args _)) =
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
        lst : _ -> (line, baseimage) == lst
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
    severity = DLErrorC
    message = "Do not use apk upgrade"
    check (Run (RunArgs args _)) = argumentsRule (Shell.noCommands (Shell.cmdHasArgs "apk" ["upgrade"])) args
    check _ = True

apkAddVersionPinned :: Rule
apkAddVersionPinned = instructionRule code severity message check
  where
    code = "DL3018"
    severity = DLWarningC
    message =
      "Pin versions in apk add. Instead of `apk add <package>` use `apk add <package>=<version>`"
    check (Run (RunArgs args _)) = argumentsRule (\as -> and [versionFixed p | p <- apkAddPackages as]) args
    check _ = True
    versionFixed package = "=" `Text.isInfixOf` package

apkAddPackages :: Shell.ParsedShell -> [Text.Text]
apkAddPackages args =
  [ arg
    | cmd <- Shell.presentCommands args,
      Shell.cmdHasArgs "apk" ["add"] cmd,
      arg <- Shell.getArgsNoFlags (dropTarget cmd),
      arg /= "add"
  ]
  where
    dropTarget = Shell.dropFlagArg ["t", "virtual", "repository", "X"]

apkAddNoCache :: Rule
apkAddNoCache = instructionRule code severity message check
  where
    code = "DL3019"
    severity = DLInfoC
    message =
      "Use the `--no-cache` switch to avoid the need to use `--update` and remove \
      \`/var/cache/apk/*` when done installing packages"
    check (Run (RunArgs args _)) = argumentsRule (Shell.noCommands forgotCacheOption) args
    check _ = True
    forgotCacheOption cmd = Shell.cmdHasArgs "apk" ["add"] cmd && not (Shell.hasFlag "no-cache" cmd)

useAdd :: Rule
useAdd = instructionRule code severity message check
  where
    code = "DL3010"
    severity = DLInfoC
    message = "Use ADD for extracting archives into an image"
    check (Copy (CopyArgs srcs _ _ _)) =
      and
        [ not (format `Text.isSuffixOf` src)
          | SourcePath src <- toList srcs,
            format <- archiveFormats
        ]
    check _ = True
    archiveFormats =
      [ ".tar",
        ".tar.bz2",
        ".tb2",
        ".tbz",
        ".tbz2",
        ".tar.gz",
        ".tgz",
        ".tpz",
        ".tar.lz",
        ".tar.lzma",
        ".tlz",
        ".tar.xz",
        ".txz",
        ".tar.Z",
        ".tZ"
      ]

invalidPort :: Rule
invalidPort = instructionRule code severity message check
  where
    code = "DL3011"
    severity = DLErrorC
    message = "Valid UNIX ports range from 0 to 65535"
    check (Expose (Ports ports)) =
      and [p <= 65535 | Port p _ <- ports]
        && and [l <= 65535 && m <= 65535 | PortRange l m _ <- ports]
    check _ = True

pipVersionPinned :: Rule
pipVersionPinned = instructionRule code severity message check
  where
    code = "DL3013"
    severity = DLWarningC
    message =
      "Pin versions in pip. Instead of `pip install <package>` use `pip install \
      \<package>==<version>` or `pip install --requirement <requirements file>`"
    check (Run (RunArgs args _)) = argumentsRule (Shell.noCommands forgotToPinVersion) args
    check _ = True
    forgotToPinVersion cmd =
      isPipInstall' cmd && not (hasBuildConstraint cmd) && not (all versionFixed (packages cmd))
    -- Check if the command is a pip* install command, and that specific packages are being listed
    isPipInstall' cmd =
      (isPipInstall cmd && not (hasBuildConstraint cmd) && not (all versionFixed (packages cmd))) && not (requirementInstall cmd)
    -- If the user is installing requirements from a file or just the local module, then we are not interested
    -- in running this rule
    requirementInstall cmd =
      ["--requirement"] `isInfixOf` Shell.getArgs cmd
        || ["-r"] `isInfixOf` Shell.getArgs cmd
        || ["."] `isInfixOf` Shell.getArgs cmd
    hasBuildConstraint cmd = Shell.hasFlag "constraint" cmd || Shell.hasFlag "c" cmd
    packages cmd =
      stripInstallPrefix $
        Shell.getArgsNoFlags $
          Shell.dropFlagArg
            [ "abi",
              "b",
              "build",
              "e",
              "editable",
              "extra-index-url",
              "f",
              "find-links",
              "i",
              "index-url",
              "implementation",
              "no-binary",
              "only-binary",
              "platform",
              "prefix",
              "progress-bar",
              "proxy",
              "python-version",
              "root",
              "src",
              "t",
              "target",
              "trusted-host",
              "upgrade-strategy"
            ]
            cmd
    versionFixed package = hasVersionSymbol package || isVersionedGit package || isLocalPackage package
    isVersionedGit package = "git+http" `Text.isInfixOf` package && "@" `Text.isInfixOf` package
    versionSymbols = ["==", ">=", "<=", ">", "<", "!=", "~=", "==="]
    hasVersionSymbol package = or [s `Text.isInfixOf` package | s <- versionSymbols]
    localPackageFileExtensions = [".whl", ".tar.gz"]
    isLocalPackage package = or [s `Text.isSuffixOf` package | s <- localPackageFileExtensions]

stripInstallPrefix :: [Text.Text] -> [Text.Text]
stripInstallPrefix cmd = dropWhile (== "install") (dropWhile (/= "install") cmd)

-- |
--  Rule for pinning NPM packages to version, tag, or commit
--  supported formats by Hadolint
--    npm install (with no args, in package dir)
--    npm install [<@scope>/]<name>
--    npm install [<@scope>/]<name>@<tag>
--    npm install [<@scope>/]<name>@<version>
--    npm install git[+http|+https]://<git-host>/<git-user>/<repo-name>[#<commit>|#semver:<semver>]
--    npm install git+ssh://<git-host>:<git-user>/<repo-name>[#<commit>|#semver:<semver>]
npmVersionPinned :: Rule
npmVersionPinned = instructionRule code severity message check
  where
    code = "DL3016"
    severity = DLWarningC
    message =
      "Pin versions in npm. Instead of `npm install <package>` use `npm install \
      \<package>@<version>`"
    check (Run (RunArgs args _)) = argumentsRule (Shell.noCommands forgotToPinVersion) args
    check _ = True
    forgotToPinVersion cmd =
      isNpmInstall cmd && installIsFirst cmd && not (all versionFixed (packages cmd))
    isNpmInstall = Shell.cmdHasArgs "npm" ["install"]
    installIsFirst cmd = ["install"] `isPrefixOf` Shell.getArgsNoFlags cmd
    packages cmd = stripInstallPrefix (Shell.getArgsNoFlags cmd)
    versionFixed package
      | hasGitPrefix package = isVersionedGit package
      | hasTarballSuffix package = True
      | isFolder package = True
      | otherwise = hasVersionSymbol package
    gitPrefixes = ["git://", "git+ssh://", "git+http://", "git+https://"]
    hasGitPrefix package = or [p `Text.isPrefixOf` package | p <- gitPrefixes]
    tarballSuffixes = [".tar", ".tar.gz", ".tgz"]
    hasTarballSuffix package = or [p `Text.isSuffixOf` package | p <- tarballSuffixes]
    pathPrefixes = ["/", "./", "../", "~/"]
    isFolder package = or [p `Text.isPrefixOf` package | p <- pathPrefixes]
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
    severity = DLWarningC
    message = "Use the `-y` switch to avoid manual input `apt-get -y install <package>`"
    check (Run (RunArgs args _)) = argumentsRule (Shell.noCommands forgotAptYesOption) args
    check _ = True
    forgotAptYesOption cmd = isAptGetInstall cmd && not (hasYesOption cmd)
    isAptGetInstall = Shell.cmdHasArgs "apt-get" ["install"]
    hasYesOption = Shell.hasAnyFlag ["y", "yes", "q", "assume-yes"]

aptGetNoRecommends :: Rule
aptGetNoRecommends = instructionRule code severity message check
  where
    code = "DL3015"
    severity = DLInfoC
    message = "Avoid additional packages by specifying `--no-install-recommends`"
    check (Run (RunArgs args _)) = argumentsRule (Shell.noCommands forgotNoInstallRecommends) args
    check _ = True
    forgotNoInstallRecommends cmd = isAptGetInstall cmd && not (disablesRecommendOption cmd)
    isAptGetInstall = Shell.cmdHasArgs "apt-get" ["install"]
    disablesRecommendOption cmd =
      Shell.hasFlag "no-install-recommends" cmd
        || Shell.hasArg "APT::Install-Recommends=false" cmd

isArchive :: Text.Text -> Bool
isArchive path =
  or
    ( [ ftype `Text.isSuffixOf` path
        | ftype <-
            [ ".tar",
              ".gz",
              ".bz2",
              ".xz",
              ".zip",
              ".tgz",
              ".tb2",
              ".tbz",
              ".tbz2",
              ".lz",
              ".lzma",
              ".tlz",
              ".txz",
              ".Z",
              ".tZ"
            ]
      ]
    )

isUrl :: Text.Text -> Bool
isUrl path = or ([proto `Text.isPrefixOf` path | proto <- ["https://", "http://"]])

copyInsteadAdd :: Rule
copyInsteadAdd = instructionRule code severity message check
  where
    code = "DL3020"
    severity = DLErrorC
    message = "Use COPY instead of ADD for files and folders"
    check (Add (AddArgs srcs _ _)) =
      and [isArchive src || isUrl src | SourcePath src <- toList srcs]
    check _ = True

copyEndingSlash :: Rule
copyEndingSlash = instructionRule code severity message check
  where
    code = "DL3021"
    severity = DLErrorC
    message = "COPY with more than 2 arguments requires the last argument to end with /"
    check (Copy (CopyArgs sources t _ _))
      | length sources > 1 = endsWithSlash t
      | otherwise = True
    check _ = True
    endsWithSlash (TargetPath t) = not (Text.null t) && Text.last t == '/'

copyFromExists :: Rule
copyFromExists dockerfile = instructionRuleLine code severity message check dockerfile
  where
    code = "DL3022"
    severity = DLWarningC
    message = "COPY --from should reference a previously defined FROM alias"
    check l (Copy (CopyArgs _ _ _ (CopySource s))) = s `elem` previouslyDefinedAliases l dockerfile
    check _ _ = True

copyFromAnother :: Rule
copyFromAnother = instructionRuleState code severity message check Nothing
  where
    code = "DL3023"
    severity = DLErrorC
    message = "COPY --from should reference a previously defined FROM alias"

    check _ _ f@(From _) = withState (Just f) True -- Remember the last FROM instruction found
    check st@(Just fromInstr) _ (Copy (CopyArgs _ _ _ (CopySource stageName))) =
      withState st (aliasMustBe (/= stageName) fromInstr) -- Cannot copy from itself!
    check state _ _ = withState state True

fromAliasUnique :: Rule
fromAliasUnique dockerfile = instructionRuleLine code severity message check dockerfile
  where
    code = "DL3024"
    severity = DLErrorC
    message = "FROM aliases (stage names) must be unique"
    check line = aliasMustBe (not . alreadyTaken line)
    alreadyTaken line alias = alias `elem` previouslyDefinedAliases line dockerfile

useShell :: Rule
useShell = instructionRule code severity message check
  where
    code = "DL4005"
    severity = DLWarningC
    message = "Use SHELL to change the default shell"
    check (Run (RunArgs args _)) = argumentsRule (Shell.noCommands (Shell.cmdHasArgs "ln" ["/bin/sh"])) args
    check _ = True

useJsonArgs :: Rule
useJsonArgs = instructionRule code severity message check
  where
    code = "DL3025"
    severity = DLWarningC
    message = "Use arguments JSON notation for CMD and ENTRYPOINT arguments"
    check (Cmd (ArgumentsText _)) = False
    check (Entrypoint (ArgumentsText _)) = False
    check _ = True

noApt :: Rule
noApt = instructionRule code severity message check
  where
    code = "DL3027"
    severity = DLWarningC
    message =
      "Do not use apt as it is meant to be a end-user tool, use apt-get or apt-cache instead"
    check (Run (RunArgs args _)) = argumentsRule (not . usingProgram "apt") args
    check _ = True

usePipefail :: Rule
usePipefail = instructionRuleState code severity message check False
  where
    code = "DL4006"
    severity = DLWarningC
    message =
      "Set the SHELL option -o pipefail before RUN with a pipe in it. If you are using \
      \/bin/sh in an alpine image or if your shell is symlinked to busybox then consider \
      \explicitly setting your SHELL to /bin/ash, or disable this check"
    check _ _ From {} = (False, True) -- Reset the state each time we find a new FROM
    check _ _ (Shell args)
      | argumentsRule isPowerShell args = (True, True)
      | otherwise = (argumentsRule hasPipefailOption args, True)
    check False _ (Run (RunArgs args _)) = (False, argumentsRule notHasPipes args)
    check st _ _ = (st, True)
    isPowerShell (Shell.ParsedShell orig _ _) = "pwsh" `Text.isPrefixOf` orig
    notHasPipes script = not (Shell.hasPipes script)
    hasPipefailOption script =
      not $
        null
          [ True
            | cmd@(Shell.Command name arguments _) <- Shell.presentCommands script,
              validShell <- ["/bin/bash", "/bin/zsh", "/bin/ash", "bash", "zsh", "ash"],
              name == validShell,
              Shell.hasFlag "o" cmd,
              arg <- Shell.arg <$> arguments,
              arg == "pipefail"
          ]

registryIsAllowed :: Set.Set Registry -> Rule
registryIsAllowed allowed = instructionRuleState code severity message check Set.empty
  where
    code = "DL3026"
    severity = DLErrorC
    message = "Use only an allowed registry in the FROM image"
    check st _ (From BaseImage {image, alias}) = withState (Set.insert alias st) (doCheck st image)
    check st _ _ = (st, True)
    toImageAlias = Just . ImageAlias . imageName

    doCheck st img = Set.member (toImageAlias img) st || Set.null allowed || isAllowed img
    isAllowed Image {registryName = Just registry} = Set.member registry allowed
    isAllowed Image {registryName = Nothing, imageName} =
      imageName == "scratch"
        || Set.member "docker.io" allowed
        || Set.member "hub.docker.com" allowed

gemVersionPinned :: Rule
gemVersionPinned = instructionRule code severity message check
  where
    code = "DL3028"
    severity = DLWarningC
    message =
      "Pin versions in gem install. Instead of `gem install <gem>` use `gem \
      \install <gem>:<version>`"
    check (Run (RunArgs args _)) = argumentsRule (all versionFixed . gems) args
    check _ = True
    versionFixed package = ":" `Text.isInfixOf` package

noPlatformFlag :: Rule
noPlatformFlag = instructionRule code severity message check
  where
    code = "DL3029"
    severity = DLWarningC
    message = "Do not use --platform flag with FROM"
    check (From BaseImage {platform = Just p}) = p == ""
    check _ = True

yumYes :: Rule
yumYes = instructionRule code severity message check
  where
    code = "DL3030"
    severity = DLWarningC
    message = "Use the -y switch to avoid manual input `yum install -y <package`"
    check (Run (RunArgs args _)) = argumentsRule (Shell.noCommands forgotYumYesOption) args
    check _ = True
    forgotYumYesOption cmd = isYumInstall cmd && not (hasYesOption cmd)
    isYumInstall = Shell.cmdHasArgs "yum" ["install", "groupinstall", "localinstall"]
    hasYesOption = Shell.hasAnyFlag ["y", "assumeyes"]

noYumUpdate :: Rule
noYumUpdate = instructionRule code severity message check
  where
    code = "DL3031"
    severity = DLErrorC
    message = "Do not use yum update."
    check (Run (RunArgs args _)) =
      argumentsRule
        ( Shell.noCommands
            ( Shell.cmdHasArgs
                "yum"
                [ "update",
                  "update-to",
                  "upgrade",
                  "upgrade-to"
                ]
            )
        )
        args
    check _ = True

yumCleanup :: Rule
yumCleanup = instructionRule code severity message check
  where
    code = "DL3032"
    severity = DLWarningC
    message = "`yum clean all` missing after yum command."
    check (Run (RunArgs args _)) =
      argumentsRule (Shell.noCommands yumInstall) args
        || ( argumentsRule (Shell.anyCommands yumInstall) args
               && argumentsRule (Shell.anyCommands yumClean) args
           )
    check _ = True
    yumInstall = Shell.cmdHasArgs "yum" ["install"]
    yumClean = Shell.cmdHasArgs "yum" ["clean", "all"]

yumVersionPinned :: Rule
yumVersionPinned = instructionRule code severity message check
  where
    code = "DL3033"
    severity = DLWarningC
    message = "Specify version with `yum install -y <package>-<version>`."
    check (Run (RunArgs args _)) = argumentsRule (all versionFixed . yumPackages) args
    check _ = True
    versionFixed package =
      "-" `Text.isInfixOf` package
        || ".rpm" `Text.isSuffixOf` package

yumPackages :: Shell.ParsedShell -> [Text.Text]
yumPackages args =
  [ arg | cmd <- Shell.presentCommands args, Shell.cmdHasArgs "yum" ["install"] cmd, arg <- Shell.getArgsNoFlags cmd, arg /= "install"
  ]

zypperYes :: Rule
zypperYes = instructionRule code severity message check
  where
    code = "DL3034"
    severity = DLWarningC
    message = "Non-interactive switch missing from `zypper` command: `zypper install -y`"
    check (Run (RunArgs args _)) = argumentsRule (Shell.noCommands forgotZypperYesOption) args
    check _ = True
    forgotZypperYesOption cmd = isZypperInstall cmd && not (hasYesOption cmd)
    isZypperInstall =
      Shell.cmdHasArgs
        "zypper"
        [ "install",
          "in",
          "remove",
          "rm",
          "source-install",
          "si",
          "patch"
        ]
    hasYesOption = Shell.hasAnyFlag ["no-confirm", "y"]

noZypperUpdate :: Rule
noZypperUpdate = instructionRule code severity message check
  where
    code = "DL3035"
    severity = DLWarningC
    message = "Do not use `zypper update`."
    check (Run (RunArgs args _)) =
      argumentsRule
        ( Shell.noCommands
            ( Shell.cmdHasArgs
                "zypper"
                [ "update",
                  "up",
                  "dist-upgrade",
                  "dup"
                ]
            )
        )
        args
    check _ = True

zypperCleanup :: Rule
zypperCleanup = instructionRule code severity message check
  where
    code = "DL3036"
    severity = DLWarningC
    message = "`zypper clean` missing after zypper use."
    check (Run (RunArgs args _)) =
      argumentsRule (Shell.noCommands zypperInstall) args
        || ( argumentsRule (Shell.anyCommands zypperInstall) args
               && argumentsRule (Shell.anyCommands zypperClean) args
           )
    check _ = True
    zypperInstall = Shell.cmdHasArgs "zypper" ["install", "in"]
    zypperClean = Shell.cmdHasArgs "zypper" ["clean", "cc"]

zypperVersionPinned :: Rule
zypperVersionPinned = instructionRule code severity message check
  where
    code = "DL3037"
    severity = DLWarningC
    message = "Specify version with `zypper install -y <package>=<version>`."
    check (Run (RunArgs args _)) = argumentsRule (all versionFixed . zypperPackages) args
    check _ = True
    versionFixed package =
      "=" `Text.isInfixOf` package
        || ">=" `Text.isInfixOf` package
        || ">" `Text.isInfixOf` package
        || "<=" `Text.isInfixOf` package
        || "<" `Text.isInfixOf` package
        || ".rpm" `Text.isSuffixOf` package

zypperPackages :: Shell.ParsedShell -> [Text.Text]
zypperPackages args =
  [ arg | cmd <- Shell.presentCommands args, Shell.cmdHasArgs "zypper" ["install", "in"] cmd, arg <- Shell.getArgsNoFlags cmd, arg /= "install", arg /= "in"
  ]

dnfYes :: Rule
dnfYes = instructionRule code severity message check
  where
    code = "DL3038"
    severity = DLWarningC
    message = "Use the -y switch to avoid manual input `dnf install -y <package`"
    check (Run (RunArgs args _)) = argumentsRule (Shell.noCommands forgotDnfYesOption) args
    check _ = True
    forgotDnfYesOption cmd = isDnfInstall cmd && not (hasYesOption cmd)
    isDnfInstall = Shell.cmdHasArgs "dnf" ["install", "groupinstall", "localinstall"]
    hasYesOption = Shell.hasAnyFlag ["y", "assumeyes"]

noDnfUpdate :: Rule
noDnfUpdate = instructionRule code severity message check
  where
    code = "DL3039"
    severity = DLErrorC
    message = "Do not use dnf update."
    check (Run (RunArgs args _)) =
      argumentsRule
        ( Shell.noCommands
            ( Shell.cmdHasArgs
                "dnf"
                [ "upgrade",
                  "upgrade-minimal"
                ]
            )
        )
        args
    check _ = True

dnfCleanup :: Rule
dnfCleanup = instructionRule code severity message check
  where
    code = "DL3040"
    severity = DLWarningC
    message = "`dnf clean all` missing after dnf command."
    check (Run (RunArgs args _)) =
      argumentsRule (Shell.noCommands dnfInstall) args
        || ( argumentsRule (Shell.anyCommands dnfInstall) args
               && argumentsRule (Shell.anyCommands dnfClean) args
           )
    check _ = True
    dnfInstall = Shell.cmdHasArgs "dnf" ["install"]
    dnfClean = Shell.cmdHasArgs "dnf" ["clean", "all"]

dnfVersionPinned :: Rule
dnfVersionPinned = instructionRule code severity message check
  where
    code = "DL3041"
    severity = DLWarningC
    message = "Specify version with `dnf install -y <package>-<version>`."
    check (Run (RunArgs args _)) = argumentsRule (all versionFixed . dnfPackages) args
    check _ = True
    versionFixed package =
      "-" `Text.isInfixOf` package
        || ".rpm" `Text.isSuffixOf` package

dnfPackages :: Shell.ParsedShell -> [Text.Text]
dnfPackages args =
  [ arg | cmd <- Shell.presentCommands args, Shell.cmdHasArgs "dnf" ["install"] cmd, arg <- Shell.getArgsNoFlags cmd, arg /= "install"
  ]

pipNoCacheDir :: Rule
pipNoCacheDir = instructionRule code severity message check
  where
    code = "DL3042"
    severity = DLWarningC
    message =
      "Avoid use of cache directory with pip. Use `pip install --no-cache-dir <package>`"
    check (Run (RunArgs args _)) = argumentsRule (Shell.noCommands forgotNoCacheDir) args
    check _ = True
    forgotNoCacheDir cmd =
      isPipInstall cmd && not (usesNoCacheDir cmd) && not (isPipWrapper cmd)
    usesNoCacheDir cmd = "--no-cache-dir" `elem` Shell.getArgs cmd

isPipInstall :: Shell.Command -> Bool
isPipInstall cmd@(Shell.Command name _ _) = isStdPipInstall || isPythonPipInstall
  where
    isStdPipInstall =
      "pip" `Text.isPrefixOf` name
        && ["install"] `isInfixOf` Shell.getArgs cmd
    isPythonPipInstall =
      "python" `Text.isPrefixOf` name
        && ["-m", "pip", "install"] `isInfixOf` Shell.getArgs cmd

isPipWrapper :: Shell.Command -> Bool
isPipWrapper cmd@(Shell.Command name _ _) = isWrapper "pipx" || isWrapper "pipenv"
  where
    isWrapper :: Text.Text -> Bool
    isWrapper w =
      w `Text.isInfixOf` name
        || ("python" `Text.isPrefixOf` name && ["-m", w] `isInfixOf` Shell.getArgs cmd)

gems :: Shell.ParsedShell -> [Text.Text]
gems shell =
  [ arg
    | cmd <- Shell.presentCommands shell,
      Shell.cmdHasArgs "gem" ["install", "i"] cmd,
      not (Shell.cmdHasArgs "gem" ["-v"] cmd),
      not (Shell.cmdHasArgs "gem" ["--version"] cmd),
      not (Shell.cmdHasPrefixArg "gem" "--version=" cmd),
      arg <- Shell.getArgsNoFlags cmd,
      arg /= "install",
      arg /= "i",
      arg /= "--"
  ]

noIllegalInstructionInOnbuild :: Rule
noIllegalInstructionInOnbuild = instructionRule code severity message check
  where
    code = "DL3043"
    severity = DLErrorC
    message = "`ONBUILD`, `FROM` or `MAINTAINER` triggered from within `ONBUILD` instruction."
    check (OnBuild (OnBuild _)) = False
    check (OnBuild (From _)) = False
    check (OnBuild (Maintainer _)) = False
    check _ = True

noSelfreferencingEnv :: Rule
noSelfreferencingEnv = instructionRuleState code severity message check Set.empty
  where
    code = "DL3044"
    severity = DLErrorC
    message = "Do not refer to an environment variable within the same `ENV` statement where it is defined."
    check st _ (Env pairs) = withState (Set.union st (Set.fromList (map fst pairs))) $
        null [ env | env <- listOfReferences pairs, env `Set.notMember` st ]
    check st _ (Arg arg _) = withState (Set.insert arg st) True
    check st _ _ = withState st True

    -- generates a list of references to variable names referenced on the right
    -- hand side of a variable definition, except when the variable is
    -- referenced on its own right hand side.
    listOfReferences :: Pairs -> [Text.Text]
    listOfReferences prs = [ var | (idx, (var, _)) <- indexed prs,
                                   var `isSubstringOfAny` map (snd . snd) (filter ((/= idx) . fst) (indexed prs))]
    -- is a reference of a variable substring of any text?
    -- matches ${var_name} and $var_name, but not $var_nameblafoo
    isSubstringOfAny :: Text.Text -> [Text.Text] -> Bool
    isSubstringOfAny t l = not $ null [ v | v <- l,
        (Text.pack "${" <> t <> Text.pack "}") `Text.isInfixOf` v
        || (t `bareVariableInText` v)]

    -- we find a 'bare' variable with name v in a text, if
    -- '$v' is in the text at any place and any text following after that
    -- occurence would terminate a variable name. To determine that, the text t
    -- is split at every occurence of var, check if '$v' is in the text and if
    -- any part of the split text would terminate a variable name.
    bareVariableInText :: Text.Text -> Text.Text -> Bool
    bareVariableInText v t =
      let var = Text.pack "$" <> v
          rest = Text.splitOn var t
       in var `Text.isInfixOf` t && any terminatesVarName rest
      where
        -- x would terminate a variable name if it was appended directly to
        -- that name
        terminatesVarName :: Text.Text -> Bool
        terminatesVarName x = not $ beginsWithAnyOf x varChar

        -- txt begins with any character of String
        beginsWithAnyOf :: Text.Text -> String -> Bool
        beginsWithAnyOf txt str = Text.null txt || (Text.head txt `elem` str)

        -- all characters valid in the inner of a shell variable name
        varChar :: String
        varChar = ['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z'] ++ ['_']
