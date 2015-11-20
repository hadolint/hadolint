module Analyzer where

import Syntax

data Rule
  = ExplicitTag
  | ExplicitMaintainer
  | SortMultilineArguments
  | MaximumNumberLayers
  | DuplicatInstruction
  | AvoidPackages
  | OneProcess
  | BuildCache
  | OfficialBaseImage
  | AptGetVersionPinning
  | AptGetCleanup
  | CmdUseExecForm
  | ChainEnv
  | NoRootUser
  | NoSudo
  | NoUpgrade
  | UseWorkdir
  | WgetAndCurlUsed
  | AbsoluteWorkdir
  | FetchInRun
  | UnusableCommand
  deriving(Show)


data Check = Success Rule | Failed Rule deriving(Show)
data Category = Error | BestPractice | Recommendation deriving(Show)

category :: Rule -> Category
category MaximumNumberLayers    = Error
category NoSudo                 = Error
category ExplicitTag            = BestPractice
category SortMultilineArguments = BestPractice
category AbsoluteWorkdir        = BestPractice
category UseWorkdir             = BestPractice
category FetchInRun             = BestPractice
category ExplicitMaintainer     = Recommendation

-- take boolean value and turn it into a failed or successful check
asCheck t True  = Success t
asCheck t False = Failed t

hasMaintainer :: Instruction -> Bool
hasMaintainer (Maintainer _) = True
hasMaintainer _              = False

explicitMaintainer :: [Instruction] -> Check
explicitMaintainer dockerfile =
    if or $ map hasMaintainer dockerfile
        then Success ExplicitMaintainer
        else Failed ExplicitMaintainer

usingCmd :: String -> Instruction -> Bool
usingCmd cmd (Run args) = elem cmd args
usingCmd _ _            = False

usingCurl :: Instruction -> Bool
usingCurl i = usingCmd "curl" i

usingWget :: Instruction -> Bool
usingWget i = usingCmd "wget" i

usingCurlAndWget  :: [Instruction] -> Check
usingCurlAndWget dockerfile =
    asCheck WgetAndCurlUsed $ anyCurl && anyWget
    where anyCurl = or $ map usingCurl dockerfile
          anyWget = or $ map usingWget dockerfile


-- check if base image has a explicit tag
explicitTag :: BaseImage -> Bool
explicitTag (LatestImage img)    = False
explicitTag (TaggedImage _ _)    = True
explicitTag (DigestedImage _ _)  = True

usingSudo :: Arguments -> Bool
usingSudo args = elem "sudo" args

-- check if cd occurs in run arguments
usingWorkdir :: Arguments -> Bool
usingWorkdir args = elem "cd" args

-- check if directory path is absolute
absoluteWorkdir :: Directory -> Bool
absoluteWorkdir dir = (head dir) == '/'

rootUser :: String -> Bool
rootUser "root" = True
rootUser  _     = False

-- check for invalid bash commands (first argument of run instruction)
isUnusable :: Arguments -> Bool
isUnusable args = elem (head args) invalidCmds
    where invalidCmds = ["ssh", "vim", "shutdown", "service", "ps", "free", "top", "kill", "mount"]

checkInstruction :: Instruction -> [Check]
checkInstruction (From instr)  = [asCheck ExplicitTag $ explicitTag instr]
checkInstruction (Workdir dir) = [asCheck AbsoluteWorkdir $ absoluteWorkdir dir]
checkInstruction (Run args)    = [asCheck UseWorkdir $ not (usingWorkdir args)
                                 ,asCheck UnusableCommand $ isUnusable args]
checkInstruction (User name)   = [asCheck NoRootUser $ not (rootUser name)]
checkInstruction _             = []

checkDockerfile :: Dockerfile -> [Check]
checkDockerfile dockerfile = nodeChecks ++ treeChecks
    where
        instructions = map instruction dockerfile
        nodeChecks = concat $ map checkInstruction $ map instruction dockerfile
        treeChecks = [explicitMaintainer instructions
                     ,usingCurlAndWget instructions]

analyzeOld :: Either t Dockerfile -> Maybe [Check]
analyzeOld (Left err) = Nothing
analyzeOld (Right d)  = Just $ checkDockerfile d

analyze :: Dockerfile -> [String]
analyze = undefined
