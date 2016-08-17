module Hadolint.Bash where

import ShellCheck.Interface
import ShellCheck.Checker
import Data.Functor.Identity (runIdentity)

shellcheck :: String -> [Comment]
shellcheck bashScript = map comment $ crComments $ runIdentity $ checkScript si spec
    where comment (PositionedComment _ c) = c
          si = mockedSystemInterface [("","")]
          spec = CheckSpec filename script exclusions (Just Bash)
          script = "#!/bin/bash\n" ++ bashScript
          filename = "" -- filename can be ommited because we only want the parse results back
          exclusions = []
