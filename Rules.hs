module Rules (Category(..), instructionRule, dockerfileRule) where

import Syntax

type InstructionCheck = Instruction -> (Maybe Bool)
type DockerfileCheck  = [Instruction] -> (Maybe Bool)

data Category = Error | BestPractice deriving(Show)
data Rule = Rule { name :: String,
                   message :: String,
                   category :: Category,
                   checkInstruction :: InstructionCheck,
                   checkDockerfile :: DockerfileCheck
                 }

noInstruction :: InstructionCheck
noInstruction _ = Nothing

noDockerfile :: DockerfileCheck
noDockerfile _ = Nothing

instructionRule  :: String -> String -> Category -> InstructionCheck -> Rule
instructionRule name msg cat check = Rule { name = name,
                                            message = msg,
                                            category = cat,
                                            checkInstruction = check,
                                            checkDockerfile = noDockerfile
                                          }

dockerfileRule :: String -> String -> Category -> DockerfileCheck -> Rule
dockerfileRule name msg cat check = Rule { name = name,
                                           message = msg,
                                           category = cat,
                                           checkInstruction = noInstruction,
                                           checkDockerfile = check
                                         }
