module Hadolint.Meta
  ( getVersion,
    getShortVersion,
  )
where

import Data.Version (showVersion)
import Development.GitRev (gitDirty, gitDescribe)
import Paths_hadolint (version)


getVersion :: String
getVersion = "Haskell Dockerfile Linter " ++ getShortVersion

getShortVersion :: String
getShortVersion = v ++ d
  where
    gitVersion = $(gitDescribe)
    cabalVersion = showVersion version
    dirty = $(gitDirty)
    v = case gitVersion of
      "UNKNOWN" -> cabalVersion ++ "-no-git"
      _ -> gitVersion
    d = if dirty then "-dirty" else ""
