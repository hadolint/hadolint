module Hadolint.Rule.DL3050 (rule) where

import qualified Data.Map as Map
import Hadolint.Rule
import Language.Docker.Syntax


rule :: LabelSchema -> Bool -> Rule args
rule labelschema strictlabels = simpleRule code severity message check
  where
    code = "DL3050"
    severity = DLInfoC
    message = "Superfluous label(s) present."
    check (Label pairs)
        | strictlabels = all ((`elem` Map.keys labelschema) . fst) pairs
        | otherwise = True
    check _ = True
{-# INLINEABLE rule #-}
