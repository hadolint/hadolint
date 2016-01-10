module Rules where

import Syntax
import Data.Maybe (isJust, fromMaybe, mapMaybe)
import Data.List (intercalate, isInfixOf, isSuffixOf)
import Data.List.Split (splitOneOf, splitOn)
import Bash

import ShellCheck.Interface
data Metadata = Metadata { code :: String,
                           severity :: Severity,
                           message :: String
                         }

-- a check is the application of a rule on a specific part of code
-- the enforced result and the affected position
-- position only records the linenumber at the moment to keep it easy
-- and simple to develop new rules
-- line numbers in the negative range are meant for the global context
data Check = Check { metadata :: Metadata,
                     linenumber :: Linenumber,
                     success :: Bool
                   }

-- a Rule takes a Dockerfile and returns the executed checks
type Rule =  Dockerfile -> [Check]

-- Apply a function on each instruction and create a check
-- for the according line number
mapInstructions :: Metadata -> (Instruction -> Bool) -> Rule
mapInstructions metadata f = map applyRule
    where applyRule (InstructionPos i linenumber) = Check metadata linenumber (f i)

instructionRule :: String -> Severity -> String -> (Instruction -> Bool) -> Rule
instructionRule code severity message f = mapInstructions (Metadata code severity message) f

dockerfileRule :: String -> Severity -> String -> ([Instruction] -> Bool) -> Rule
dockerfileRule code severity message f = rule
    where rule dockerfile = [Check metadata (-1) (f (map instruction dockerfile))]
          metadata = Metadata code severity message

-- Enforce rules on a dockerfile and return failed checks
analyze :: [Rule] -> Dockerfile -> [Check]
analyze rules dockerfile = filter failed $ concat [r dockerfile | r <- rules]
    where failed (Check _ _ success) = not success

rules = [ absoluteWorkdir
        , shellcheckBash
        , hasMaintainer
        , maintainerAddress
        , wgetOrCurl
        , invalidCmd
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
        ]

commentMetadata :: ShellCheck.Interface.Comment -> Metadata
commentMetadata (ShellCheck.Interface.Comment severity code message) = Metadata ("SC" ++ show code) severity message

shellcheckBash :: Dockerfile -> [Check]
shellcheckBash dockerfile = concat $ map check dockerfile
    where check (InstructionPos (Run args) linenumber) = [Check m linenumber False | m <- convert args]
          check _ = []
          convert args = [commentMetadata c | c <- shellcheck $ unwords args]

-- Split different bash commands
bashCommands :: [String] -> [[String]]
bashCommands = splitOneOf [";", "|", "&&"]

absoluteWorkdir = instructionRule code severity message check
    where code = "DL3000"
          severity = ErrorC
          message = "Use absolute WORKDIR"
          check (Workdir dir) = head dir == '/'
          check _ = True

hasMaintainer = dockerfileRule code severity message check
    where code = "DL4000"
          severity = InfoC
          message = "Specify a maintainer of the Dockerfile"
          check = any maintainer
          maintainer (Maintainer _) = True
          maintainer _              = False

-- Check if a command contains a program call in the Run instruction
usingProgram prog args = or [head cmds == prog | cmds <- bashCommands args]

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
          message = "Always tag the version of an image explicitely."
          check (From (UntaggedImage _)) = False
          check (From (TaggedImage _ _)) = True
          check _ = True

noLatestTag = instructionRule code severity message check
    where code = "DL3007"
          severity = WarningC
          message = "Using latest is prone to errors if the image will ever update. Pin the version explicitely to a release tag."
          check (From (TaggedImage _ "latest")) = False
          check (From (TaggedImage _ _)) = True
          check _ = True

aptGetVersionPinned = instructionRule code severity message check
    where code = "DL3008"
          severity = WarningC
          message = "Pin versions in apt get install. Instead of `apt-get install <package>` use `apt-get install <package>=<version>`"
          check (Run args) = and [versionFixed p | p <- packages args]
          check _ = True
          versionFixed package = "=" `isInfixOf` package
          packages :: [String] -> [String]
          packages args = concat [drop 2 cmd | cmd <- bashCommands args, isInstall cmd]
          isInstall cmd = ["apt-get", "install"] `isInfixOf` cmd

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

invalidPort = instructionRule code severity message check
    where code = "DL3011"
          severity = ErrorC
          message = "Valid UNIX ports range from 0 to 65535"
          check (Expose ports) = and [p <= 65535 | p <- ports]
          check _ = True

maintainerAddress = instructionRule code severity message check
    where code = "DL3012"
          severity = StyleC
          message = "Provide an email adress or URL as maintainer"
          check (Maintainer name) = isInfixOf "@" name || isInfixOf "http://" name
          check _ = True

pipVersionPinned = instructionRule code severity message check
    where code = "DL3013"
          severity = WarningC
          message = "Pin versions in pip. Instead of `pip install <package>` use `pip install <package>==<version>`"
          check (Run args) = not (isPipInstall args && not (isRecursiveInstall args)) ||
                                all versionFixed (packages args)
          check _ = True
          versionFixed package = "==" `isInfixOf` package
          packages :: [String] -> [String]
          packages args = concat [drop 2 cmd | cmd <- bashCommands args, isPipInstall cmd]
          isPipInstall cmd = ["pip", "install"] `isInfixOf` cmd
          isRecursiveInstall cmd = ["-r"] `isInfixOf` cmd

aptGetYes = instructionRule code severity message check
    where code = "DL3014"
          severity = WarningC
          message = "Use the `-y` switch to avoid manual input `apt-get -y install <package>`"
          check (Run args) = not (isInstall args) || hasYesOption args
          check _ = True
          hasYesOption cmd = ["-y"] `isInfixOf` cmd || ["--yes"] `isInfixOf` cmd
          isInstall cmd = ["apt-get", "install"] `isInfixOf` cmd

aptGetNoRecommends = instructionRule code severity message check
    where code = "DL3015"
          severity = InfoC
          message = "Avoid additional packages by specifying `--no-install-recommends`"
          check (Run args) = not (isInstall args) || hasNoRecommendsOption args
          check _ = True
          hasNoRecommendsOption cmd = ["--no-install-recommends"] `isInfixOf` cmd
          isInstall cmd = ["apt-get", "install"] `isInfixOf` cmd
