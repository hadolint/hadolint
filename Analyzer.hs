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

explicitTag :: BaseImage -> Check
explicitTag (LatestImage img)           = Success ExplicitTag
explicitTag (TaggedImage img tag)       = Failed ExplicitTag
explicitTag (DigestedImage img digest)  = Failed ExplicitTag

checkInstruction :: Instruction -> Maybe Check
checkInstruction (From instr) = Just $ explicitTag instr
checkInstruction _            = Nothing

-- Run checks that apply to the entire AST tree and not on a single nod
checkDockerfile :: Dockerfile -> [Maybe Check]
checkDockerfile dockerfile = map checkInstruction dockerfile

analyze :: Either t Dockerfile -> Maybe [Maybe Check]
analyze (Left err) = Nothing
analyze (Right d)  = Just $ checkDockerfile d
