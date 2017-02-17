module Hadolint.Rules where

import Hadolint.Syntax
import Hadolint.Bash
import Data.Maybe (isJust, fromMaybe, mapMaybe)
import Data.List (intercalate, isInfixOf, isSuffixOf, isPrefixOf)
import Data.List.Split (splitOneOf, splitOn)

import ShellCheck.Interface
data Metadata = Metadata { code :: String,
                           severity :: Severity,
                           message :: String
                         } deriving (Eq)

-- a check is the application of a rule on a specific part of code
-- the enforced result and the affected position
-- position only records the linenumber at the moment to keep it easy
-- and simple to develop new rules
-- line numbers in the negative range are meant for the global context
data Check = Check { metadata :: Metadata,
                     filename :: Filename,
                     linenumber :: Linenumber,
                     success :: Bool
                   } deriving Eq

instance Ord Check where
    a `compare` b = linenumber a `compare` linenumber b

link :: Metadata -> String
link (Metadata code _ _)
    | "SC" `isPrefixOf` code  = "https://github.com/koalaman/shellcheck/wiki/" ++ code
    | "DL" `isPrefixOf` code  = "https://github.com/lukasmartinelli/hadolint/wiki/" ++ code
    | otherwise               = "https://github.com/lukasmartinelli/hadolint"

-- a Rule takes a Dockerfile and returns the executed checks
type Rule =  Dockerfile -> [Check]

-- Apply a function on each instruction and create a check
-- for the according line number
mapInstructions :: Metadata -> (Instruction -> Bool) -> Rule
mapInstructions metadata f = map applyRule
    where applyRule (InstructionPos i source linenumber) = Check metadata source linenumber (f i)

instructionRule :: String -> Severity -> String -> (Instruction -> Bool) -> Rule
instructionRule code severity message f = mapInstructions (Metadata code severity message) f

dockerfileRule :: String -> Severity -> String -> ([Instruction] -> Bool) -> Rule
dockerfileRule code severity message f = rule
    where rule dockerfile = [Check metadata (filename dockerfile) (-1) (f (map instruction dockerfile))]
          metadata = Metadata code severity message
          filename dockerfile = sourcename $ head dockerfile

-- Enforce rules on a dockerfile and return failed checks
analyze :: [Rule] -> Dockerfile -> [Check]
analyze rules dockerfile = filter failed $ concat [r dockerfile | r <- rules]
    where failed (Check _ _ _ success) = not success

rules = [ absoluteWorkdir
        , shellcheckBash
        , invalidCmd
        , copyInsteadAdd
        , noRootUser
        , noCd
        , noSudo
        , noUpgrade
        , noLatestTag
        , noUntagged
        , aptGetVersionPinned
        , aptGetCleanup
        , useAdd
        , pipVersionPinned
        , invalidPort
        , aptGetNoRecommends
        , aptGetYes
        , wgetOrCurl
        , hasNoMaintainer
        , multipleCmds
        , multipleEntrypoints
        , useShell
        , exposeMissingArgs
        , copyMissingArgs
        ]

commentMetadata :: ShellCheck.Interface.Comment -> Metadata
commentMetadata (ShellCheck.Interface.Comment severity code message) = Metadata ("SC" ++ show code) severity message

shellcheckBash :: Dockerfile -> [Check]
shellcheckBash dockerfile = concatMap check dockerfile
    where check (InstructionPos (Run args) source linenumber) = rmDup [Check m source linenumber False | m <- convert args]
          check _ = []
          convert args = [commentMetadata c | c <- shellcheck $ unwords args]
          rmDup :: [Check] -> [Check]
          rmDup [] = []
          rmDup (x:xs) = x : rmDup (filter (\y -> metadata x /= metadata y) xs)

-- Split different bash commands
bashCommands :: [String] -> [[String]]
bashCommands = splitOneOf [";", "|", "&&"]

absoluteWorkdir = instructionRule code severity message check
    where code = "DL3000"
          severity = ErrorC
          message = "Use absolute WORKDIR"
          check (Workdir dir) = head dir == '$' || head dir == '/'
          check _ = True

hasNoMaintainer = dockerfileRule code severity message check
    where code = "DL4000"
          severity = ErrorC
          message = "MAINTAINER is deprecated"
          check dockerfile = not $ any isMaintainer dockerfile
          isMaintainer (Maintainer _) = True
          isMaintainer _              = False

-- Check if a command contains a program call in the Run instruction
usingProgram prog args = or [head cmds == prog | cmds <- bashCommands args]

multipleCmds =dockerfileRule code severity message check
    where code = "DL4003"
          severity = WarningC
          message = "Multiple `CMD` instructions found. Only the first instruction will take effect."
          check dockerfile = 1 >= length (filter (True==) $ map isCmd dockerfile)
          isCmd (Cmd _) = True
          isCmd _       = False

multipleEntrypoints =dockerfileRule code severity message check
    where code = "DL4004"
          severity = ErrorC
          message = "Multiple `ENTRYPOINT` instructions found. Only the first instruction will take effect."
          check dockerfile = 1 >= length (filter (True==) $ map isEntrypoint dockerfile)
          isEntrypoint (Entrypoint _) = True
          isEntrypoint _       = False

wgetOrCurl = dockerfileRule code severity message check
    where code = "DL4001"
          severity = WarningC
          message = "Either use Wget or Curl but not both"
          check dockerfile = not $ anyCurl dockerfile && anyWget dockerfile
          anyCurl = any $ usingCmd "curl"
          anyWget = any $ usingCmd "wget"
          usingCmd cmd (Run args) = cmd `elem` args
          usingCmd _ _            = False


invalidCmd = instructionRule code severity message check
    where code = "DL3001"
          severity = InfoC
          message = "For some bash commands it makes no sense running them in a Docker container like `ssh`, `vim`, `shutdown`, `service`, `ps`, `free`, `top`, `kill`, `mount`, `ifconfig`"
          check (Run args) = head args `notElem` invalidCmds
          check _ = True
          invalidCmds = ["ssh", "vim", "shutdown", "service", "ps", "free", "top", "kill", "mount"]

noRootUser = instructionRule code severity message check
    where code = "DL3002"
          severity = ErrorC
          message = "Do not switch to root USER"
          check (User "root") = False
          check (User _) = True
          check _ = True

noCd = instructionRule code severity message check
    where code = "DL3003"
          severity = WarningC
          message = "Use WORKDIR to switch to a directory"
          check (Run args) = not $ usingProgram "cd" args
          check _ = True

noSudo = instructionRule code severity message check
    where code = "DL3004"
          severity = ErrorC
          message = "Do not use sudo as it leads to unpredictable behavior. Use a tool like gosu to enforce root."
          check (Run args) = not $ usingProgram "sudo" args
          check _ = True

noUpgrade = instructionRule code severity message check
    where code = "DL3005"
          severity = ErrorC
          message = "Do not use apt-get upgrade or dist-upgrade."
          check (Run args) = not $ isInfixOf ["apt-get", "upgrade"] args
          check _ = True

noUntagged = instructionRule code severity message check
    where code = "DL3006"
          severity = WarningC
          message = "Always tag the version of an image explicitly."
          check (From (UntaggedImage image)) = image == "scratch"
          check (From (TaggedImage _ _)) = True
          check _ = True

noLatestTag = instructionRule code severity message check
    where code = "DL3007"
          severity = WarningC
          message = "Using latest is prone to errors if the image will ever update. Pin the version explicitly to a release tag."
          check (From (TaggedImage _ "latest")) = False
          check (From (TaggedImage _ _)) = True
          check _ = True

aptGetVersionPinned = instructionRule code severity message check
    where code = "DL3008"
          severity = WarningC
          message = "Pin versions in apt get install. Instead of `apt-get install <package>` use `apt-get install <package>=<version>`"
          check (Run args) = and [versionFixed p | p <- aptGetPackages args]
          check _ = True
          versionFixed package = "=" `isInfixOf` package

aptGetPackages :: [String] -> [String]
aptGetPackages args = concat [filter noOption cmd | cmd <- bashCommands args, isAptGetInstall cmd]
    where noOption arg = arg `notElem` options && not ("--" `isPrefixOf` arg)
          options = [ "-f" , "install" , "apt-get" , "-d" , "-f" , "-m" , "-q" , "-y" ]

aptGetCleanup = instructionRule code severity message check
    where code = "DL3009"
          severity = InfoC
          message = "Delete the apt-get lists after installing something"
          check (Run args) = not (hasUpdate args) || hasCleanup args
          check _ = True
          hasCleanup cmd = ["rm", "-rf", "/var/lib/apt/lists/*"] `isInfixOf` cmd
          hasUpdate cmd = ["apt-get", "update"] `isInfixOf` cmd

useAdd = instructionRule code severity message check
    where code = "DL3010"
          severity = InfoC
          message = "Use ADD for extracting archives into an image"
          check (Copy src dst) = and [not (format `isSuffixOf` src) | format <- archive_formats]
          check _ = True
          archive_formats = [".tar", ".gz", ".bz2", "xz"]

exposeMissingArgs = instructionRule code severity message check
    where code = "DL3021"
          severity = ErrorC
          message = "EXPOSE requires at least one argument"
          check (Expose ports) = length ports > 0
          check _ = True

copyMissingArgs = instructionRule code severity message check
    where code = "DL3022"
          severity = ErrorC
          message = "COPY requires source and target"
          check (Copy src target) = length src > 0 && length target > 0
          check _ = True

invalidPort = instructionRule code severity message check
    where code = "DL3011"
          severity = ErrorC
          message = "Valid UNIX ports range from 0 to 65535"
          check (Expose ports) = and [p <= 65535 | p <- ports]
          check _ = True

pipVersionPinned = instructionRule code severity message check
    where code = "DL3013"
          severity = WarningC
          message = "Pin versions in pip. Instead of `pip install <package>` use `pip install <package>==<version>`"
          check (Run args) = not (isPipInstall args && not (isRecursiveInstall args)) ||
                                all versionFixed (packages args)
          check _ = True
          isVersionedGit :: String -> Bool
          isVersionedGit package = "git+http" `isInfixOf` package && "@" `isInfixOf` package

          versionSymbols = ["==", ">=", "<=", ">", "<", "!="]
          hasVersionSymbol :: String -> Bool
          hasVersionSymbol package = or [s `isInfixOf` package | s <- versionSymbols]

          versionFixed :: String -> Bool
          versionFixed package = hasVersionSymbol package || isVersionedGit package

          packages :: [String] -> [String]
          packages args = concat [drop 2 cmd | cmd <- bashCommands args, isPipInstall cmd]

          isPipInstall :: [String] -> Bool
          isPipInstall cmd = ["pip", "install"] `isInfixOf` cmd || ["pip3", "install"] `isInfixOf` cmd || ["pip2", "install"] `isInfixOf` cmd

          isRecursiveInstall:: [String] -> Bool
          isRecursiveInstall cmd = ["-r"] `isInfixOf` cmd


isAptGetInstall cmd = ["apt-get"] `isInfixOf` cmd && ["install"] `isInfixOf` cmd
aptGetYes = instructionRule code severity message check
    where code = "DL3014"
          severity = WarningC
          message = "Use the `-y` switch to avoid manual input `apt-get -y install <package>`"
          check (Run args) = not (isAptGetInstall args) || hasYesOption args
          check _ = True
          hasYesOption cmd = ["-y"] `isInfixOf` cmd || ["--yes"] `isInfixOf` cmd || startsWithYesFlag cmd
          startsWithYesFlag cmd = True `elem` ["-y" `isInfixOf` arg | arg <- cmd]

aptGetNoRecommends = instructionRule code severity message check
    where code = "DL3015"
          severity = InfoC
          message = "Avoid additional packages by specifying `--no-install-recommends`"
          check (Run args) = not (isAptGetInstall args) || hasNoRecommendsOption args
          check _ = True
          hasNoRecommendsOption cmd = ["--no-install-recommends"] `isInfixOf` cmd

isArchive :: String -> Bool
isArchive path =  True `elem` [ftype `isSuffixOf` path | ftype <- [".tar", ".gz", ".bz2", ".xz", ".zip", ".tgz", ".tb2", ".tbz", ".tbz2", ".lz", ".lzma", ".tlz", ".txz", ".Z", ".tZ"]]
isUrl :: String -> Bool
isUrl path = True `elem` [proto `isPrefixOf` path | proto <- ["https://", "http://"]]
copyInsteadAdd = instructionRule code severity message check
    where code = "DL3020"
          severity = ErrorC
          message = "Use COPY instead of ADD for files and folders"
          check (Add src _) = isArchive src || isUrl src
          check _ = True

useShell = instructionRule code severity message check
    where code = "DL4005"
          severity = WarningC
          message = "Use SHELL to change the default shell"
          check (Run args) = not $ any shellSymlink (bashCommands args)
          check _ = True
          shellSymlink args = usingProgram "ln" args && isInfixOf ["/bin/sh"] args
