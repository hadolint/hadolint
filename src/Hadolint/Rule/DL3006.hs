module Hadolint.Rule.DL3006 (rule) where

import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Text (Text)
import Hadolint.Rule
import Language.Docker.Syntax

rule :: Rule args
rule = customRule check (emptyState Set.empty)
  where
    code = "DL3006"
    severity = DLWarningC
    message = "Always tag the version of an image explicitly"

    check line st (From from) =
      let newState = st |> modify (insertFromAlias from)
       in case from of
            BaseImage {image = (Image _ "scratch")} -> newState
            BaseImage {digest = Just _} -> newState
            BaseImage {image = (Image _ i), tag = Nothing} ->
              -- When the image being used is a previously defined FROM alias,
              -- then we can safely ignore that the image is not tagged. Otherwise
              -- we marked it as a failure
              if Text.isPrefixOf "$" i || Set.member i (state st)
                then newState
                else newState |> addFail (CheckFailure {..})
            _ -> newState
    check _ st _ = st
{-# INLINEABLE rule #-}

insertFromAlias :: BaseImage -> Set.Set Text -> Set.Set Text
insertFromAlias BaseImage {alias = Just a} st = st |> Set.insert (unImageAlias a)
insertFromAlias _ st = st
