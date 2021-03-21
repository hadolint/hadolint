module Hadolint.Rule.DL3054 (rule) where

import Data.Either
import qualified Data.Map as Map
import qualified Data.Text as Text
import Distribution.Parsec
import Distribution.SPDX.Extra
import Hadolint.Rule
import Language.Docker.Syntax


rule :: LabelSchema -> Rule args
rule labelschema = mconcat $
  fmap labelIsNotSPDXRule (Map.keys (Map.filter (== Spdx) labelschema))
{-# INLINEABLE rule #-}

labelIsNotSPDXRule :: LabelName -> Rule args
labelIsNotSPDXRule label = simpleRule code severity message check
  where
    code = "DL3054"
    severity = DLWarningC
    message = "Label `" <> label <> "` is not a valid SPDX identifier."
    check (Label ls) = null $ getBadLicenseLabels label ls
    check _ = True


getBadLicenseLabels :: LabelName -> Pairs -> Pairs
getBadLicenseLabels lbl pairs =
    [ (l, v) | (l, v) <- pairs,
               l == lbl,
               isLeft (eitherParsec (Text.unpack v) :: Either String LicenseId)
    ]
