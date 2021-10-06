module Hadolint.Config
  ( CommandlineConfig (..),
    Configuration (..),
    getConfigFromEnvironment,
    getConfigFromFile,
    parseCommandline
  )
where

import Hadolint.Config.Commandline
import Hadolint.Config.Configuration
import Hadolint.Config.Environment
import Hadolint.Config.Configfile
