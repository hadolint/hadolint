module Hadolint.Rules where

import Data.List (intercalate, isInfixOf, isPrefixOf, isSuffixOf)
import Data.List.NonEmpty (toList)
import Data.List.Split (splitOn, splitOneOf)
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Hadolint.Bash
import Language.Docker.Syntax

import ShellCheck.Interface

data Metadata = Metadata
    { code :: String
    , severity :: Severity
    , message :: String
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

link :: Metadata -> String
link (Metadata code _ _)
    | "SC" `isPrefixOf` code = "https://github.com/koalaman/shellcheck/wiki/" ++ code
    | "DL" `isPrefixOf` code = "https://github.com/hadolint/hadolint/wiki/" ++ code
    | otherwise = "https://github.com/hadolint/hadolint"

-- a Rule takes a Dockerfile and returns the executed checks
type Rule = Dockerfile -> [RuleCheck]

-- Apply a function on each instruction and create a check
-- for the according line number
mapInstructions :: Metadata -> (Instruction -> Bool) -> Rule
mapInstructions metadata f = map applyRule
  where
    applyRule (InstructionPos i source linenumber) = RuleCheck metadata source linenumber (f i)

instructionRule :: String -> Severity -> String -> (Instruction -> Bool) -> Rule
instructionRule code severity message = mapInstructions $ Metadata code severity message

dockerfileRule :: String -> Severity -> String -> ([Instruction] -> Bool) -> Rule
dockerfileRule code severity message f = rule
  where
    rule dockerfile =
        [RuleCheck metadata (filename dockerfile) (-1) (f (map instruction dockerfile))]
    metadata = Metadata code severity message
    filename dockerfile = sourcename $ head dockerfile

-- Enforce rules on a dockerfile and return failed checks
analyze :: [Rule] -> Dockerfile -> [RuleCheck]
analyze rules dockerfile = filter failed $ concat [r dockerfile | r <- rules]
  where
    failed (RuleCheck _ _ _ success) = not success

rules =
    [ absoluteWorkdir
    , shellcheckBash
    , invalidCmd
    , copyInsteadAdd
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
    Metadata ("SC" ++ show code) severity message

shellcheckBash :: Dockerfile -> [RuleCheck]
shellcheckBash = concatMap check
  where
    check (InstructionPos (Run args) source linenumber) =
        rmDup [RuleCheck m source linenumber False | m <- convert args]
    check _ = []
    convert args = [commentMetadata c | c <- shellcheck $ unwords args]
    rmDup :: [RuleCheck] -> [RuleCheck]
    rmDup [] = []
    rmDup (x:xs) = x : rmDup (filter (\y -> metadata x /= metadata y) xs)

-- Split different bash commands
bashCommands :: [String] -> [[String]]
bashCommands = splitOneOf [";", "|", "&&"]

absoluteWorkdir = instructionRule code severity message check
  where
    code = "DL3000"
    severity = ErrorC
    message = "Use absolute WORKDIR"
    check (Workdir dir) = head dir == '$' || head dir == '/'
    check _ = True

hasNoMaintainer = dockerfileRule code severity message check
  where
    code = "DL4000"
    severity = ErrorC
    message = "MAINTAINER is deprecated"
    check dockerfile = not $ any isMaintainer dockerfile
    isMaintainer (Maintainer _) = True
    isMaintainer _ = False

-- Check if a command contains a program call in the Run instruction
usingProgram prog args = or [head cmds == prog | cmds <- bashCommands args]

multipleCmds = dockerfileRule code severity message check
  where
    code = "DL4003"
    severity = WarningC
    message =
        "Multiple `CMD` instructions found. If you list more than one `CMD` then only the last \
        \`CMD` will take effect."
    check dockerfile = 1 >= length (filter (True ==) $ map isCmd dockerfile)
    isCmd (Cmd _) = True
    isCmd _ = False

multipleEntrypoints = dockerfileRule code severity message check
  where
    code = "DL4004"
    severity = ErrorC
    message =
        "Multiple `ENTRYPOINT` instructions found. If you list more than one `ENTRYPOINT` then \
        \only the last `ENTRYPOINT` will take effect."
    check dockerfile = 1 >= length (filter (True ==) $ map isEntrypoint dockerfile)
    isEntrypoint (Entrypoint _) = True
    isEntrypoint _ = False

wgetOrCurl = dockerfileRule code severity message check
  where
    code = "DL4001"
    severity = WarningC
    message = "Either use Wget or Curl but not both"
    check dockerfile = not $ anyCurl dockerfile && anyWget dockerfile
    anyCurl = any $ usingCmd "curl"
    anyWget = any $ usingCmd "wget"
    usingCmd cmd (Run args) = cmd `elem` args
    usingCmd _ _ = False

invalidCmd = instructionRule code severity message check
  where
    code = "DL3001"
    severity = InfoC
    message =
        "For some bash commands it makes no sense running them in a Docker container like `ssh`, \
        \`vim`, `shutdown`, `service`, `ps`, `free`, `top`, `kill`, `mount`, `ifconfig`"
    check (Run args) = head args `notElem` invalidCmds
    check _ = True
    invalidCmds = ["ssh", "vim", "shutdown", "service", "ps", "free", "top", "kill", "mount"]

noRootUser = instructionRule code severity message check
  where
    code = "DL3002"
    severity = WarningC
    message = "Do not switch to root USER"
    check (User user) =
        not (isPrefixOf "root:" user || isPrefixOf "0:" user || user == "root" || user == "0")
    check _ = True

noCd = instructionRule code severity message check
  where
    code = "DL3003"
    severity = WarningC
    message = "Use WORKDIR to switch to a directory"
    check (Run args) = not $ usingProgram "cd" args
    check _ = True

noSudo = instructionRule code severity message check
  where
    code = "DL3004"
    severity = ErrorC
    message =
        "Do not use sudo as it leads to unpredictable behavior. Use a tool like gosu to enforce \
        \root."
    check (Run args) = not $ usingProgram "sudo" args
    check _ = True

noAptGetUpgrade = instructionRule code severity message check
  where
    code = "DL3005"
    severity = ErrorC
    message = "Do not use apt-get upgrade or dist-upgrade."
    check (Run args) = not $ isInfixOf ["apt-get", "upgrade"] args
    check _ = True

noUntagged = instructionRule code severity message check
  where
    code = "DL3006"
    severity = WarningC
    message = "Always tag the version of an image explicitly."
    check (From (UntaggedImage image _)) = image == "scratch"
    check (From TaggedImage {}) = True
    check _ = True

noLatestTag = instructionRule code severity message check
  where
    code = "DL3007"
    severity = WarningC
    message =
        "Using latest is prone to errors if the image will ever update. Pin the version explicitly \
        \to a release tag."
    check (From (TaggedImage _ tag _)) = tag /= "latest"
    check _ = True

aptGetVersionPinned = instructionRule code severity message check
  where
    code = "DL3008"
    severity = WarningC
    message =
        "Pin versions in apt get install. Instead of `apt-get install <package>` use `apt-get \
        \install <package>=<version>`"
    check (Run args) = and [versionFixed p | p <- aptGetPackages args]
    check _ = True
    versionFixed package = "=" `isInfixOf` package

aptGetPackages :: [String] -> [String]
aptGetPackages args = concat [filter noOption cmd | cmd <- bashCommands args, isAptGetInstall cmd]
  where
    noOption arg = arg `notElem` options && not ("--" `isPrefixOf` arg)
    options = ["apt-get", "install", "-d", "-f", "-m", "-q", "-y", "-qq"]

aptGetCleanup = instructionRule code severity message check
  where
    code = "DL3009"
    severity = InfoC
    message = "Delete the apt-get lists after installing something"
    check (Run args) = not (hasUpdate args) || hasCleanup args
    check _ = True
    hasCleanup cmd = ["rm", "-rf", "/var/lib/apt/lists/*"] `isInfixOf` cmd
    hasUpdate cmd = ["apt-get", "update"] `isInfixOf` cmd

dropOptionsWithArg :: [String] -> [String] -> [String]
dropOptionsWithArg os [] = []
dropOptionsWithArg os (x:xs)
    | x `elem` os = dropOptionsWithArg os (drop 1 xs)
    | otherwise = x : dropOptionsWithArg os xs

noApkUpgrade = instructionRule code severity message check
  where
    code = "DL3017"
    severity = ErrorC
    message = "Do not use apk upgrade"
    check (Run args) = not $ isInfixOf ["apk", "upgrade"] args
    check _ = True

isApkAdd :: [String] -> Bool
isApkAdd cmd = ["apk"] `isInfixOf` cmd && ["add"] `isInfixOf` cmd

apkAddVersionPinned = instructionRule code severity message check
  where
    code = "DL3018"
    severity = WarningC
    message =
        "Pin versions in apk add. Instead of `apk add <package>` use `apk add <package>=<version>`"
    check (Run args) = and [versionFixed p | p <- apkAddPackages args]
    check _ = True
    versionFixed package = "=" `isInfixOf` package

apkAddPackages :: [String] -> [String]
apkAddPackages args =
    concat
        [ filter noOption (dropOptionsWithArg ["-t", "--virtual"] cmd)
        | cmd <- bashCommands args
        , isApkAdd cmd
        ]
  where
    noOption arg = arg `notElem` options && not ("--" `isPrefixOf` arg)
    options = ["apk", "add", "-q", "-p", "-v", "-f", "-t"]

apkAddNoCache = instructionRule code severity message check
  where
    code = "DL3019"
    severity = InfoC
    message =
        "Use the `--no-cache` switch to avoid the need to use `--update` and remove \
        \`/var/cache/apk/*` when done installing packages"
    check (Run args) = not (isApkAdd args) || hasNoCacheOption args
    check _ = True
    hasNoCacheOption cmd = ["--no-cache"] `isInfixOf` cmd

useAdd = instructionRule code severity message check
  where
    code = "DL3010"
    severity = InfoC
    message = "Use ADD for extracting archives into an image"
    check (Copy (CopyArgs srcs _ _ _)) =
        and
            [ not (format `isSuffixOf` src)
            | SourcePath src <- toList srcs
            , format <- archiveFormats
            ]
    check _ = True
    archiveFormats = [".tar", ".gz", ".bz2", "xz"]

invalidPort = instructionRule code severity message check
  where
    code = "DL3011"
    severity = ErrorC
    message = "Valid UNIX ports range from 0 to 65535"
    check (Expose (Ports ports)) =
        and [p <= 65535 | Port p _ <- ports] &&
        and [l <= 65535 && m <= 65535 | PortRange l m <- ports]
    check _ = True

pipVersionPinned = instructionRule code severity message check
  where
    code = "DL3013"
    severity = WarningC
    message =
        "Pin versions in pip. Instead of `pip install <package>` use `pip install \
        \<package>==<version>`"
    check (Run args) =
        not (isPipInstall args && not (isRecursiveInstall args)) || all versionFixed (packages args)
    check _ = True
    isVersionedGit :: String -> Bool
    isVersionedGit package = "git+http" `isInfixOf` package && "@" `isInfixOf` package
    versionSymbols = ["==", ">=", "<=", ">", "<", "!="]
    hasVersionSymbol :: String -> Bool
    hasVersionSymbol package = or [s `isInfixOf` package | s <- versionSymbols]
    versionFixed :: String -> Bool
    versionFixed package = hasVersionSymbol package || isVersionedGit package
    isPipInstall :: [String] -> Bool
    isPipInstall cmd =
        ["pip", "install"] `isInfixOf` cmd ||
        ["pip3", "install"] `isInfixOf` cmd || ["pip2", "install"] `isInfixOf` cmd
    isRecursiveInstall :: [String] -> Bool
    isRecursiveInstall cmd = ["-r"] `isInfixOf` cmd
    -- | Returns all the packages after pip install
    packages :: [String] -> [String]
    packages args = concat [filter noOption cmd | cmd <- bashCommands args, isPipInstall cmd]
      where
        noOption arg = arg `notElem` commandName && not (isFlag arg)
        commandName = ["pip", "pip2", "pip3", "install"]
        isFlag ('-':_) = True
        isFlag _ = False

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
npmVersionPinned = instructionRule code severity message check
  where
    code = "DL3016"
    severity = WarningC
    message =
        "Pin versions in npm. Instead of `npm install <package>` use `npm install \
        \<package>@<version>`"
    check (Run args) = all versionFixed (packages args)
    check _ = True
    packages :: [String] -> [String]
    packages args = concat [filter noOption cmd | cmd <- bashCommands args, isNpmInstall cmd]
      where
        noOption arg = arg `notElem` options && not ("--" `isPrefixOf` arg)
        options = ["npm", "install", "-g", "-f"]
    isNpmInstall :: [String] -> Bool
    isNpmInstall cmd = ["npm", "install"] `isInfixOf` cmd
    versionFixed :: String -> Bool
    versionFixed package =
        if hasGitPrefix package
            then isVersionedGit package
            else hasVersionSymbol package
    gitPrefixes = ["git://", "git+ssh://", "git+http://", "git+https://"]
    hasGitPrefix :: String -> Bool
    hasGitPrefix package = or [p `isPrefixOf` package | p <- gitPrefixes]
    isVersionedGit :: String -> Bool
    isVersionedGit package = "#" `isInfixOf` package
    hasVersionSymbol :: String -> Bool
    hasVersionSymbol package = "@" `isInfixOf` dropScope package
      where
        dropScope package =
            if "@" `isPrefixOf` package
                then dropWhile ('/' <) package
                else package

isAptGetInstall cmd = ["apt-get"] `isInfixOf` cmd && ["install"] `isInfixOf` cmd

aptGetYes = instructionRule code severity message check
  where
    code = "DL3014"
    severity = WarningC
    message = "Use the `-y` switch to avoid manual input `apt-get -y install <package>`"
    check (Run args) = not (isAptGetInstall args) || hasYesOption args
    check _ = True
    hasYesOption cmd =
        ["-y"] `isInfixOf` cmd ||
        ["--yes"] `isInfixOf` cmd || ["-qq"] `isInfixOf` cmd || startsWithYesFlag cmd
    startsWithYesFlag cmd = True `elem` ["-y" `isInfixOf` arg | arg <- cmd]

aptGetNoRecommends = instructionRule code severity message check
  where
    code = "DL3015"
    severity = InfoC
    message = "Avoid additional packages by specifying `--no-install-recommends`"
    check (Run args) = not (isAptGetInstall args) || hasNoRecommendsOption args
    check _ = True
    hasNoRecommendsOption cmd = ["--no-install-recommends"] `isInfixOf` cmd

isArchive :: String -> Bool
isArchive path =
    True `elem`
    [ ftype `isSuffixOf` path
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

isUrl :: String -> Bool
isUrl path = True `elem` [proto `isPrefixOf` path | proto <- ["https://", "http://"]]

copyInsteadAdd = instructionRule code severity message check
  where
    code = "DL3020"
    severity = ErrorC
    message = "Use COPY instead of ADD for files and folders"
    check (Add (AddArgs srcs _ _)) =
        and [isArchive src || isUrl src | SourcePath src <- toList srcs]
    check _ = True

useShell = instructionRule code severity message check
  where
    code = "DL4005"
    severity = WarningC
    message = "Use SHELL to change the default shell"
    check (Run args) = not $ any shellSymlink (bashCommands args)
    check _ = True
    shellSymlink args = usingProgram "ln" args && isInfixOf ["/bin/sh"] args
