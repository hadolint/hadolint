module Rules (Rule(..), instructionRule, dockerfileRule) where

import Syntax

type InstructionCheck = Instruction -> Maybe Bool
type DockerfileCheck  = [Instruction] -> Maybe Bool

data Rule = Rule { name :: String,
                   message :: String,
                   checkInstruction :: InstructionCheck,
                   checkDockerfile :: DockerfileCheck
                 }

instance Show Rule where
    show (Rule name _ _ _) = "Rule " ++ show name

noInstruction :: InstructionCheck
noInstruction _ = Nothing

noDockerfile :: DockerfileCheck
noDockerfile _ = Nothing

instructionRule  :: String -> String -> InstructionCheck -> Rule
instructionRule name msg check = Rule { name = name,
                                        message = msg,
                                        checkInstruction = check,
                                        checkDockerfile = noDockerfile
                                      }

dockerfileRule :: String -> String -> DockerfileCheck -> Rule
dockerfileRule name msg check = Rule { name = name,
                                       message = msg,
                                       checkInstruction = noInstruction,
                                       checkDockerfile = check
                                     }
