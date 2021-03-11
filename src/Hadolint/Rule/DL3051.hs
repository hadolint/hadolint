module Hadolint.Rule.DL3051 (rule) where

import qualified Data.Map as Map
import qualified Data.Text as Text
import Hadolint.Rule
import Language.Docker.Syntax


rule :: LabelSchema -> Rule args
rule labelschema = mconcat $ fmap labelIsNotEmptyRule (Map.keys labelschema)
{-# INLINEABLE rule #-}

labelIsNotEmptyRule :: LabelName -> Rule args
labelIsNotEmptyRule label = simpleRule code severity message check
  where
    code = "DL3051"
    severity = DLWarningC
    message = "label `" <> label <> "` is empty."
    check (Label pairs) = null $ getEmptyLabels label pairs
    check _ = True

getEmptyLabels :: LabelName -> Pairs -> Pairs
getEmptyLabels lbl prs = [(l, v) | (l, v) <- prs, l == lbl, Text.null v]
