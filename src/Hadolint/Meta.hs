module Hadolint.Meta
  ( getVersion,
    getShortVersion,
  )
where

import qualified Development.GitRev


getVersion :: String
getVersion = "Haskell Dockerfile Linter " ++ getShortVersion

getShortVersion :: String
getShortVersion = v ++ d
  where
    version = $(Development.GitRev.gitDescribe)
    dirty = $(Development.GitRev.gitDirty)
    v = case version of
      "UNKONWN" -> "-no-git"
      _ -> version
    d = if dirty then "-dirty" else ""
