module Analyzer where

import Syntax

data Rule
  = ExplicitTag
  | ExplicitMaintainer
  deriving(Show)


data Check = Success Rule | Failed Rule deriving(Show)
data Category = Error | BestPractice deriving(Show)

category :: Rule -> Category
category ExplicitTag        = BestPractice
category ExplicitMaintainer = BestPractice

hasMaintainer :: Instruction -> Bool
hasMaintainer (Maintainer _) = True
hasMaintainer _              = False

explicitMaintainer :: Dockerfile -> Check
explicitMaintainer dockerfile =
    if or $ map hasMaintainer dockerfile
        then Success ExplicitMaintainer
        else Failed ExplicitMaintainer

explicitTag :: BaseImage -> Check
explicitTag (LatestImage img)           = Success ExplicitTag
explicitTag (TaggedImage img tag)       = Failed ExplicitTag
explicitTag (DigestedImage img digest)  = Failed ExplicitTag

checkInstruction :: Instruction -> [Check]
checkInstruction (From instr) = [explicitTag instr]
checkInstruction _            = []

checkDockerfile :: Dockerfile -> [Check]
checkDockerfile dockerfile = nodeChecks ++ treeChecks
    where nodeChecks = concat $ map checkInstruction dockerfile
          treeChecks = [explicitMaintainer dockerfile]

analyze :: Either t Dockerfile -> Maybe [Check]
analyze (Left err) = Nothing
analyze (Right d)  = Just $ checkDockerfile d
