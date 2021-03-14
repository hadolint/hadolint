module Hadolint.Rule.DL3020 (rule) where

import Data.Foldable (toList)
import qualified Data.Text as Text
import Hadolint.Rule
import Language.Docker.Syntax

rule :: Rule args
rule = simpleRule code severity message check
  where
    code = "DL3020"
    severity = DLErrorC
    message = "Use COPY instead of ADD for files and folders"

    check (Add (AddArgs srcs _ _)) =
      and [isArchive src || isUrl src | SourcePath src <- toList srcs]
    check _ = True
{-# INLINEABLE rule #-}

isArchive :: Text.Text -> Bool
isArchive path =
  or
    ( [ ftype `Text.isSuffixOf` path
        | ftype <-
            [ ".tar",
              ".gz",
              ".bz2",
              ".xz",
              ".zip",
              ".tgz",
              ".tb2",
              ".tbz",
              ".tbz2",
              ".lz",
              ".lzma",
              ".tlz",
              ".txz",
              ".Z",
              ".tZ"
            ]
      ]
    )

isUrl :: Text.Text -> Bool
isUrl path = or ([proto `Text.isPrefixOf` path | proto <- ["https://", "http://"]])
