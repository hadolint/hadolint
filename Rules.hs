module Rules where

import Syntax

data Rule
    = AbsoluteWorkdir
    | InvalidCmd
    | NoRootUser
    | NoSudo
    | NoCd
    | NoUpgrade
    | WgetOrCurl
    | HasMaintainer

data Category = Error | BestPractice deriving(Show)

class Check a where
    message :: a -> String
    category :: a -> Category
    checkInstruction :: a -> Instruction -> (Maybe Bool)
    checkDockerfile :: a -> [Instruction] -> (Maybe Bool)

usingCmd :: String -> Instruction -> Bool
usingCmd cmd (Run args) = elem cmd args
usingCmd _ _            = False

instance Check Rule where
    category NoRootUser = BestPractice
    category NoCd = BestPractice
    category NoSudo = BestPractice
    category NoUpgrade = BestPractice
    category HasMaintainer = BestPractice
    category AbsoluteWorkdir = BestPractice
    category InvalidCmd = BestPractice
    category WgetOrCurl = BestPractice

    message WgetOrCurl = "Either use curl or wget for downloading"
    message HasMaintainer = "Specify a maintainer of the Dockerfile"
    message AbsoluteWorkdir = "Use absolute WORKDIR"
    message InvalidCmd = "For some bash commands it makes no sense running them in a Docker container like `ssh`, `vim`, `shutdown`, `service`, `ps`, `free`, `top`, `kill`, `mount`, `ifconfig`"
    message NoRootUser = "Do not switch to root USER"
    message NoCd = "Use WORKDIR to switch to a directory"
    message NoSudo = "Do not use sudo as it leads to unpredictable behavior. Use a tool like gosu to enforce root."
    message NoUpgrade = "Do not use apt-get upgrade or dist-upgrade."

    checkInstruction AbsoluteWorkdir (Workdir dir) = Just $ not $ (head dir) == '/'
    checkInstruction InvalidCmd (Run args) = Just $ not $ elem (head args) invalidCmds
        where invalidCmds = ["ssh", "vim", "shutdown", "service", "ps", "free", "top", "kill", "mount"]
    checkInstruction NoRootUser (User "root") = Just $ False
    checkInstruction NoRootUser (User _) = Just $ True
    checkInstruction NoCd (Run args) = Just $ not $ elem "cd" args
    checkInstruction NoSudo (Run args) = Just $ not $ elem "sudo" args
    checkInstruction NoUpgrade (Run args) = Just $ True
    checkInstruction _ _ = Nothing

    checkDockerfile WgetOrCurl dockerfile = Just $ not $ anyCurl && anyWget
        where anyCurl = or $ map usingCurl dockerfile
              anyWget = or $ map usingWget dockerfile
              usingCurl i = usingCmd "curl" i
              usingWget i = usingCmd "wget" i
    checkDockerfile HasMaintainer dockerfile = Just $ or $ map hasMaintainer dockerfile
        where hasMaintainer (Maintainer _) = True
              hasMaintainer _              = False
    checkDockerfile _ _ = Nothing
