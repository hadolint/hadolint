module Analyzer(analyze, appliedChecks, failedChecks, successfulChecks, Check(..)) where

import Syntax
import Rules
import Data.Maybe (isJust, fromMaybe)
import Data.List (intercalate, isInfixOf)
import Data.List.Split (splitOneOf, splitOn)

data Check = DockerfileCheck Rule Dockerfile | InstructionCheck Rule InstructionPos

instance Show Check where
    show (DockerfileCheck rule _) = unwords ["DockerfileCheck", show rule]
    show (InstructionCheck rule pos) = unwords ["InstructionCheck", show rule, show pos]

-- Execute rule of check on related dockerfile or instruction
eval :: Check -> Maybe Bool
eval (DockerfileCheck rule dockerfile) = (checkDockerfile rule) (map instruction dockerfile)
eval (InstructionCheck rule pos) = (checkInstruction rule) (instruction pos)

-- Analyze a dockerfile and apply all checks to instructions and dockerfile
analyze :: Dockerfile -> [Check]
analyze dockerfile = dockerfileChecks ++ instructionChecks
    where dockerfileChecks = [DockerfileCheck r dockerfile | r <- rules]
          instructionChecks = [InstructionCheck r pos | r <- rules, pos <- dockerfile]

appliedChecks checks = [c | c <- checks, isJust (eval c)]
failedChecks checks = [c | c <- appliedChecks checks, (fromMaybe False (eval c)) == False]
successfulChecks checks = [c | c <- appliedChecks checks, fromMaybe False (eval c)]

-- Split different bash commands
bashCommands :: [String] -> [[String]]
bashCommands args = splitOneOf [";", "|", "&&"] args

-- Documentation for each rule
rulesDocs = unlines [name r ++ "\t " ++ message r | r <- rules]

rules = [ absoluteWorkdir
        , hasMaintainer
        , wgetOrCurl
        , invalidCmd
        , noRootUser
        , noCd
        , noSudo
        , noUpgrade
        , noLatestTag
        , noUntagged
        , aptGetVersionPinned
        ]

absoluteWorkdir = instructionRule name msg check
    where name = "AbsoluteWorkdir"
          msg = "Use absolute WORKDIR"
          check (Workdir dir) = Just $ head dir == '/'
          check _ = Nothing

hasMaintainer = dockerfileRule name msg check
    where name = "HasMaintainer"
          msg = "Specify a maintainer of the Dockerfile"
          check dockerfile = Just $ or $ map maintainer dockerfile
          maintainer (Maintainer _) = True
          maintainer _              = False

-- Check if a command contains a program call in the Run instruction
usingProgram prog args = or $ [(head cmds) == prog | cmds <- bashCommands args]

wgetOrCurl = dockerfileRule name msg check
    where name = "WgetOrCurl"
          msg = "Either use Wget or Curl but not both"
          check dockerfile = Just $ not $ anyCurl dockerfile && anyWget dockerfile
          anyCurl dockerfile = or $ map usingCurl dockerfile
          anyWget dockerfile = or $ map usingWget dockerfile
          usingCurl i = usingCmd "curl" i
          usingWget i = usingCmd "wget" i
          usingCmd cmd (Run args) = elem cmd args
          usingCmd _ _            = False


invalidCmd = instructionRule name msg check
    where name = "InvalidCmd"
          msg = "For some bash commands it makes no sense running them in a Docker container like `ssh`, `vim`, `shutdown`, `service`, `ps`, `free`, `top`, `kill`, `mount`, `ifconfig`"
          check (Run args) = Just $ notElem (head args) invalidCmds
          check _ = Nothing
          invalidCmds = ["ssh", "vim", "shutdown", "service", "ps", "free", "top", "kill", "mount"]

noRootUser = instructionRule name msg check
    where name = "NoRoot"
          msg = "Do not switch to root USER"
          check (User "root") = Just False
          check (User _) = Just True
          check _ = Nothing

noCd = instructionRule name msg check
    where name ="NoCd"
          msg = "Use WORKDIR to switch to a directory"
          check (Run args) = Just $ not $ usingProgram "cd" args
          check _ = Nothing

noSudo = instructionRule name msg check
    where name = "NoSudo"
          msg = "Do not use sudo as it leads to unpredictable behavior. Use a tool like gosu to enforce root."
          check (Run args) = Just $ not $ usingProgram "sudo" args
          check _ = Nothing

noUpgrade = instructionRule name msg check
    where name = "NoUpgrade"
          msg = "Do not use apt-get upgrade or dist-upgrade."
          check (Run args) = Just $ not $ isInfixOf ["apt-get", "upgrade"] args
          check _ = Nothing

noUntagged = instructionRule name msg check
    where name = "NoUntagged"
          msg = "Always tag the version of an image explicitely."
          check (From (UntaggedImage _)) = Just $ False
          check (From (TaggedImage _ _)) = Just $ True
          check _ = Nothing

noLatestTag = instructionRule name msg check
    where name = "NoLatestTag"
          msg = "Using latest is prone to errors if the image will ever update. Pin the version explicitely to a release tag."
          check (From (TaggedImage _ "latest")) = Just $ False
          check (From (TaggedImage _ _)) = Just $ True
          check _ = Nothing

aptGetVersionPinned = instructionRule name msg check
    where name = "AptGetVersionPinning"
          msg = "Pin versions in apt get install. Instead of `apt-get install <package>` use `apt-get install <package>=<version>`"
          check (Run args) = Just $ and $ [versionFixed p | p <- packages args]
          check _ = Nothing
          versionFixed package = isInfixOf "=" package
          packages :: [String] -> [String]
          packages args = concat [drop 2 cmd | cmd <- bashCommands args, isInstall cmd]
          isInstall cmd = isInfixOf ["apt-get", "install"] cmd
