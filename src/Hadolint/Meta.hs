module Hadolint.Meta
  ( getVersion,
    getShortVersion,
  )
where

import Data.Version (showVersion)
import Paths_hadolint (version)


getVersion :: String
getVersion = "Haskell Dockerfile Linter " ++ getShortVersion

getShortVersion :: String
getShortVersion = showVersion version
