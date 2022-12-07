module Hadolint.Rule.DL3058 (rule) where

import qualified Data.Map as Map
import Text.Email.Validate as Email
import Hadolint.Rule
import Language.Docker.Syntax
import Data.Text.Encoding


rule :: LabelSchema -> Rule args
rule labelschema = mconcat $
  fmap labelIsNotEmailRule (Map.keys (Map.filter (== Email) labelschema))
{-# INLINEABLE rule #-}


labelIsNotEmailRule :: LabelName -> Rule args
labelIsNotEmailRule label = simpleRule code severity message check
  where
    code = "DL3058"
    severity = DLWarningC
    message = "Label `" <> label <> "` is not a valid email format - must conform to RFC5322."
    check (Label ls) = null $ getBadEmailformatLabels label ls
    check _ = True

getBadEmailformatLabels :: LabelName -> Pairs -> Pairs
getBadEmailformatLabels lbl pairs = [(l, v) | (l, v) <- pairs, l == lbl, not (Email.isValid (encodeUtf8 v))]
