module Hadolint.Rule.DL3053 (rule) where

import qualified Data.Map as Map
import Data.Maybe
import Data.Time.RFC3339
import Hadolint.Rule
import Language.Docker.Syntax


rule :: LabelSchema -> Rule args
rule labelschema = mconcat $
  fmap labelIsNotRFC3339Rule (Map.keys (Map.filter (== Rfc3339) labelschema))
{-# INLINEABLE rule #-}


labelIsNotRFC3339Rule :: LabelName -> Rule args
labelIsNotRFC3339Rule label = simpleRule code severity message check
  where
    code = "DL3053"
    severity = DLWarningC
    message = "Label `" <> label <> "` is not a valid time format - must be conform to RFC3339."
    check (Label ls) = null $ getBadTimeformatLabels label ls
    check _ = True

getBadTimeformatLabels :: LabelName -> Pairs -> Pairs
getBadTimeformatLabels lbl pairs = [(l, v) | (l, v) <- pairs,
                                             l == lbl,
                                             isNothing $ parseTimeRFC3339 v]
