module Rules where

import Syntax
import Data.Maybe (isJust, fromMaybe, mapMaybe)
import Data.List (intercalate, isInfixOf)
import Data.List.Split (splitOneOf, splitOn)

-- a check is the application of a rule which returns
-- the enforcement result of a rule and the affected line numbers
type Check = (Rule, RuleResult)

-- result of a enforced rule on a specific line
-- line numbers in the negative range are meant for the global context
type RuleResult = (Linenumber, Bool)

-- a rule has a short name and a message that describes the reason for the rule
-- the check functions take in a Dockerfile and return a check
data Rule = Rule { name :: String,
                   message :: String,
                   enforce:: Dockerfile -> [RuleResult]
                 }

instance Show Rule where
    show (Rule name _ _) = "Rule " ++ show name


-- Apply a function on each instruction and create a check
-- for the according line number if function returns a state
mapInstructions :: (Instruction -> Bool) -> Dockerfile -> [RuleResult]
mapInstructions f dockerfile = map applyRule dockerfile
    where applyRule (InstructionPos i linenumber) = (linenumber, f i)

instructionRule :: String -> String -> (Instruction -> Bool) -> Rule
instructionRule name message f = Rule { name = name,
                                        message = message,
                                        enforce = mapInstructions f
                                      }

dockerfileRule :: String -> String -> ([Instruction] -> Bool) -> Rule
dockerfileRule name message f = Rule { name = name,
                                       message = message,
                                       enforce = enforce
                                     }
    where enforce :: Dockerfile -> [RuleResult]
          enforce dockerfile = [(-1, f (map instruction dockerfile))]

-- Enforce rules on a dockerfile and return failed checks
analyze :: [Rule] -> Dockerfile -> [Check]
analyze rules dockerfile = concat $ map unwrapChecks enforcedRules
    where unwrapChecks (r, checks) = [(r, c) | c <- checks]
          enforcedRules = [(r, failedChecks $ enforce r dockerfile) | r <- rules]
          failedChecks checks  = filter failed checks
          failed (_, success) = not success

allRules = suggestionRules ++ bestPracticeRules
suggestionRules = [ hasMaintainer ]
bestPracticeRules = [ absoluteWorkdir
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

-- Documentation for each rule
rulesDocs = unlines [name r ++ "\t " ++ message r | r <- allRules]


-- Split different bash commands
bashCommands :: [String] -> [[String]]
bashCommands args = splitOneOf [";", "|", "&&"] args

absoluteWorkdir = instructionRule name message check
    where name = "AbsoluteWorkdir"
          message = "Use absolute WORKDIR"
          check (Workdir dir) = head dir == '/'
          check _ = True

hasMaintainer = dockerfileRule name message check
    where name = "HasMaintainer"
          message = "Specify a maintainer of the Dockerfile"
          check dockerfile = or $ map maintainer dockerfile
          maintainer (Maintainer _) = True
          maintainer _              = False

-- Check if a command contains a program call in the Run instruction
usingProgram prog args = or $ [(head cmds) == prog | cmds <- bashCommands args]

wgetOrCurl = dockerfileRule name message check
    where name = "WgetOrCurl"
          message = "Either use Wget or Curl but not both"
          check dockerfile = not $ anyCurl dockerfile && anyWget dockerfile
          anyCurl dockerfile = or $ map usingCurl dockerfile
          anyWget dockerfile = or $ map usingWget dockerfile
          usingCurl i = usingCmd "curl" i
          usingWget i = usingCmd "wget" i
          usingCmd cmd (Run args) = elem cmd args
          usingCmd _ _            = False


invalidCmd = instructionRule name message check
    where name = "InvalidCmd"
          message = "For some bash commands it makes no sense running them in a Docker container like `ssh`, `vim`, `shutdown`, `service`, `ps`, `free`, `top`, `kill`, `mount`, `ifconfig`"
          check (Run args) = notElem (head args) invalidCmds
          check _ = True
          invalidCmds = ["ssh", "vim", "shutdown", "service", "ps", "free", "top", "kill", "mount"]

noRootUser = instructionRule name message check
    where name = "NoRoot"
          message = "Do not switch to root USER"
          check (User "root") = False
          check (User _) = True
          check _ = True

noCd = instructionRule name message check
    where name ="NoCd"
          message = "Use WORKDIR to switch to a directory"
          check (Run args) = not $ usingProgram "cd" args
          check _ = True

noSudo = instructionRule name message check
    where name = "NoSudo"
          message = "Do not use sudo as it leads to unpredictable behavior. Use a tool like gosu to enforce root."
          check (Run args) = not $ usingProgram "sudo" args
          check _ = True

noUpgrade = instructionRule name message check
    where name = "NoUpgrade"
          message = "Do not use apt-get upgrade or dist-upgrade."
          check (Run args) = not $ isInfixOf ["apt-get", "upgrade"] args
          check _ = True

noUntagged = instructionRule name message check
    where name = "NoUntagged"
          message = "Always tag the version of an image explicitely."
          check (From (UntaggedImage _)) = False
          check (From (TaggedImage _ _)) = True
          check _ = True

noLatestTag = instructionRule name message check
    where name = "NoLatestTag"
          message = "Using latest is prone to errors if the image will ever update. Pin the version explicitely to a release tag."
          check (From (TaggedImage _ "latest")) = False
          check (From (TaggedImage _ _)) = True
          check _ = True

aptGetVersionPinned = instructionRule name message check
    where name = "AptGetVersionPinning"
          message = "Pin versions in apt get install. Instead of `apt-get install <package>` use `apt-get install <package>=<version>`"
          check (Run args) = and $ [versionFixed p | p <- packages args]
          check _ = True
          versionFixed package = isInfixOf "=" package
          packages :: [String] -> [String]
          packages args = concat [drop 2 cmd | cmd <- bashCommands args, isInstall cmd]
          isInstall cmd = isInfixOf ["apt-get", "install"] cmd

aptGetCleanup = instructionRule name message check
    where name = "AptGetCleanup"
          message = "Delete the apt-get lists after installing something"
          check (Run args) = if hasUpdate args then hasCleanup args else True
          check _ = True
          hasCleanup cmd = isInfixOf ["rm", "-rf", "/var/lib/apt/lists/*"] cmd
          hasUpdate cmd = isInfixOf ["apt-get", "update"] cmd
