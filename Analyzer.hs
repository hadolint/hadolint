module Analyzer(analyze, appliedChecks, failedChecks, successfulChecks, Check(..)) where

import Syntax
import Rules
import Data.Maybe (isJust, fromMaybe)
import Data.List (intercalate, isInfixOf)

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
        ]

absoluteWorkdir = instructionRule name msg category check
    where name = "AbsoluteWorkdir"
          msg = "Use absolute WORKDIR"
          category = BestPractice
          check (Workdir dir) = Just $ head dir == '/'
          check _ = Nothing

hasMaintainer = dockerfileRule name msg category check
    where name = "HasMaintainer"
          msg = "Specify a maintainer of the Dockerfile"
          category = BestPractice
          check dockerfile = Just $ or $ map maintainer dockerfile
          maintainer (Maintainer _) = True
          maintainer _              = False


wgetOrCurl = dockerfileRule name msg category check
    where name = "WgetOrCurl"
          msg = "Either use Wget or Curl but not both"
          category = BestPractice
          check dockerfile = Just $ not $ anyCurl dockerfile && anyWget dockerfile
          anyCurl dockerfile = or $ map usingCurl dockerfile
          anyWget dockerfile = or $ map usingWget dockerfile
          usingCurl i = usingCmd "curl" i
          usingWget i = usingCmd "wget" i
          usingCmd cmd (Run args) = elem cmd args
          usingCmd _ _            = False


invalidCmd = instructionRule name msg category check
    where name = "InvalidCmd"
          msg = "For some bash commands it makes no sense running them in a Docker container like `ssh`, `vim`, `shutdown`, `service`, `ps`, `free`, `top`, `kill`, `mount`, `ifconfig`"
          category = BestPractice
          check (Run args) = Just $ notElem (head args) invalidCmds
          check _ = Nothing
          invalidCmds = ["ssh", "vim", "shutdown", "service", "ps", "free", "top", "kill", "mount"]

noRootUser = instructionRule name msg category check
    where name = "NoRoot"
          msg = "Do not switch to root USER"
          category = BestPractice
          check (User "root") = Just False
          check (User _) = Just True
          check _ = Nothing

noCd = instructionRule name msg category check
    where name ="NoCd"
          msg = "Use WORKDIR to switch to a directory"
          category = BestPractice
          check (Run args) = Just $ notElem "cd" args
          check _ = Nothing

noSudo = instructionRule name msg category check
    where name = "NoSudo"
          msg = "Do not use sudo as it leads to unpredictable behavior. Use a tool like gosu to enforce root."
          category = BestPractice
          check (Run args) = Just $ notElem (head args) ["sudo"]
          check _ = Nothing

noUpgrade = instructionRule name msg category check
    where name = "NoUpgrade"
          msg = "Do not use apt-get upgrade or dist-upgrade."
          category = BestPractice
          check (Run args) = Just $ not $ isInfixOf ["apt-get", "upgrade"] args
          check _ = Nothing

noLatestTag = instructionRule name msg category check
    where name = "NoLatestTag"
          msg = "Using latest is prone to errors if the image will ever update. Pin the version explicitely to a release tag."
          category = BestPractice
          check (From (LatestImage _)) = Just $ False
          check (From (TaggedImage _ "latest")) = Just $ False
          check (From (TaggedImage _ _)) = Just $ True
          check _ = Nothing
