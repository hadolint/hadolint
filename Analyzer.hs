module Analyzer where

import Syntax
import Rules

absoluteWorkdir = instructionRule name msg category check
    where name = "AbsoluteWorkdir"
          msg = "Use absolute WORKDIR"
          category = BestPractice
          check (Workdir dir) = Just $ (head dir) == '/'

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
          check (Run args) = Just $ not $ elem (head args) invalidCmds
          invalidCmds = ["ssh", "vim", "shutdown", "service", "ps", "free", "top", "kill", "mount"]

noRootUser = instructionRule name msg category check
    where name = "NoRoot"
          msg = "Do not switch to root USER"
          category = BestPractice
          check (User "root") = Just $ False
          check (User _) = Just $ True

noCd = instructionRule name msg category check
    where name ="NoCd"
          msg = "Use WORKDIR to switch to a directory"
          category = BestPractice
          check (Run args) = Just $ not $ elem "cd" args

noSudo = instructionRule name msg category check
    where name = "NoSudo"
          msg = "Do not use sudo as it leads to unpredictable behavior. Use a tool like gosu to enforce root."
          category = BestPractice
          check (Run args) = Just $ not $ elem "sudo" args

noUpgrade = instructionRule name msg category check
    where name = "NoUpgrade"
          msg = "Do not use apt-get upgrade or dist-upgrade."
          category = BestPractice
          check (Run args) = Just $ True

