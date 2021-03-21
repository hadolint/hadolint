module Hadolint.Rule.DL3052 (rule) where

import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Text as Text
import Hadolint.Rule
import Language.Docker.Syntax
import Network.URI as Uri


rule :: LabelSchema -> Rule args
rule labelschema = mconcat $
  fmap labelIsNotUrlRule (Map.keys (Map.filter (== Url) labelschema))
{-# INLINEABLE rule #-}

labelIsNotUrlRule :: LabelName -> Rule args
labelIsNotUrlRule label = simpleRule code severity message check
  where
    code = "DL3052"
    severity = DLWarningC
    message = "Label `" <> label <> "` is not a valid URL."
    check (Label ls) = null $ getBadURLLabels label ls
    check _ = True

getBadURLLabels :: LabelName -> Pairs -> Pairs
getBadURLLabels lbl prs = [(l, u) | (l, u) <- prs, l == lbl,
                                    (isNothing . Uri.parseURI) (Text.unpack u)]
