module Analyzer where

import Syntax

data Rule
  = ExplicitTag
  | ExplicitMaintainer
  | SortMultilineArguments
  | MaximumNumberLayers
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
  | UseWorkdir
  | AbsoluteWorkdir
  deriving(Show)


data Check = Success Rule | Failed Rule deriving(Show)
data Category = Error | BestPractice deriving(Show)

category :: Rule -> Category
category MaximumNumberLayers    = Error
category NoSudo                 = Error
category ExplicitTag            = BestPractice
category ExplicitMaintainer     = BestPractice
category SortMultilineArguments = BestPractice
category AbsoluteWorkdir        = BestPractice
category UseWorkdir             = BestPractice

-- take boolean value and turn it into a failed or successful check
asCheck t True  = Success t
asCheck t False = Failed t

hasMaintainer :: Instruction -> Bool
hasMaintainer (Maintainer _) = True
hasMaintainer _              = False

explicitMaintainer :: Dockerfile -> Check
explicitMaintainer dockerfile =
    if or $ map hasMaintainer dockerfile
        then Success ExplicitMaintainer
        else Failed ExplicitMaintainer

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

checkInstruction :: Instruction -> [Check]
checkInstruction (From instr)  = [asCheck ExplicitTag $ explicitTag instr]
checkInstruction (Workdir dir) = [asCheck AbsoluteWorkdir $ absoluteWorkdir dir]
checkInstruction (Run args)    = [asCheck UseWorkdir $ usingWorkdir args]
checkInstruction _             = []

checkDockerfile :: Dockerfile -> [Check]
checkDockerfile dockerfile = nodeChecks ++ treeChecks
    where nodeChecks = concat $ map checkInstruction dockerfile
          treeChecks = [explicitMaintainer dockerfile]

analyze :: Either t Dockerfile -> Maybe [Check]
analyze (Left err) = Nothing
analyze (Right d)  = Just $ checkDockerfile d
