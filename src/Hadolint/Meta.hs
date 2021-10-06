module Hadolint.Meta
  ( getVersion,
    getShortVersion,
    (<>>)
  )
where


import Data.Maybe (isNothing)
import qualified Development.GitRev


infixl 0 <>>

(<>>) :: Maybe Bool -> Maybe Bool -> Maybe Bool
b1 <>> b2
  | isNothing b2 = b1
  | otherwise = b2

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
