module Rules where

import Syntax

data Category = Error | BestPractice deriving(Show)

class Rule a where
    message :: a -> String
    category :: a -> Category

    checkInstruction :: a -> Instruction -> (Maybe Bool)
    checkInstruction _ _ = Nothing

    checkDockerfile :: a -> [Instruction] -> (Maybe Bool)
    checkDockerfile _ _ = Nothing

usingCmd :: String -> Instruction -> Bool
usingCmd cmd (Run args) = elem cmd args
usingCmd _ _            = False

data WgetOrCurl
instance Rule WgetOrCurl where
    message _ = "Either use curl or wget for downloading"
    category _ = BestPractice
    checkDockerfile _ dockerfile = Just $ not $ anyCurl && anyWget
        where anyCurl = or $ map usingCurl dockerfile
              anyWget = or $ map usingWget dockerfile
              usingCurl i = usingCmd "curl" i
              usingWget i = usingCmd "wget" i

data HasMaintainer
instance Rule HasMaintainer where
    message _ = "Specify a maintainer of the Dockerfile"
    category _ = BestPractice
    checkDockerfile _ dockerfile = Just $ or $ map hasMaintainer dockerfile
        where hasMaintainer (Maintainer _) = True
              hasMaintainer _              = False


data AbsoluteWorkdir
instance Rule AbsoluteWorkdir where
    message _ = "Use absolute WORKDIR"
    category _ = BestPractice
    checkInstruction _ (Workdir dir) = Just $ not $ (head dir) == '/'


data InvalidCmd
instance Rule InvalidCmd where
    message _ = "For some bash commands it makes no sense running them in a Docker container like `ssh`, `vim`, `shutdown`, `service`, `ps`, `free`, `top`, `kill`, `mount`, `ifconfig`"
    category _ = BestPractice
    checkInstruction _ (Run args) = Just $ not $ elem (head args) invalidCmds
        where invalidCmds = ["ssh", "vim", "shutdown", "service", "ps", "free", "top", "kill", "mount"]

data NoRootUser
instance Rule NoRootUser where
    message _ = "Do not switch to root USER"
    category _ = BestPractice
    checkInstruction _ (User "root") = Just $ False
    checkInstruction _ (User _) = Just $ True

data NoCd = NoCd
instance Rule NoCd where
    message _ = "Use WORKDIR to switch to a directory"
    category _ = BestPractice
    checkInstruction _ (Run args) = Just $ not $ elem "cd" args

data NoSudo = NoSudo
instance Rule NoSudo where
    message _ = "Do not use sudo as it leads to unpredictable behavior. Use a tool like gosu to enforce root."
    category _ = BestPractice
    checkInstruction _ (Run args) = Just $ not $ elem "sudo" args

data NoUpgrade = NoUpgrade
instance Rule NoUpgrade where
    message _ = "Do not use apt-get upgrade or dist-upgrade."
    category _ = BestPractice
    checkInstruction _ (Run args) = Just $ True

msg = message NoUpgrade
