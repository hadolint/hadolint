module Hadolint.Rule.DL3056 (rule) where

import Data.Either
import qualified Data.Map as Map
import qualified Data.SemVer as SemVer
import qualified Data.Text as Text
import Hadolint.Rule
import Language.Docker.Syntax


rule :: LabelSchema -> Rule args
rule labelschema = mconcat $
  fmap labelIsNotSemVerRule (Map.keys (Map.filter (== SemVer) labelschema))
{-# INLINEABLE rule #-}

labelIsNotSemVerRule :: LabelName -> Rule args
labelIsNotSemVerRule label = simpleRule code severity message check
  where
    code = "DL3056"
    severity = DLWarningC
    message = Text.pack "Label `" <> label <> Text.pack "` does not conform to semantic versioning."
    check (Label pairs) = hasNoBadVersioning label pairs
    check _ = True

hasNoBadVersioning :: LabelName -> Pairs -> Bool
hasNoBadVersioning lbl prs = null [(l, v) | (l, v) <- prs, l == lbl,
                                            isLeft $ SemVer.fromText v]
