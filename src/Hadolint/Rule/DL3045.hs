module Hadolint.Rule.DL3045 (rule) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Hadolint.Rule
import Language.Docker.Syntax

-- This data encapsulates the name of a build stage. It may be None withing an
-- `ONBUILD` context.
data Stage
  = Stage {stage :: Text}
  | None
  deriving (Eq, Ord)

-- | The state here keeps the image name/alias of the current build stage
-- and a map from image names/aliases to a Bool, saving whether or
-- not a `WORKDIR` has been set in a build stage.
data Acc
  = Acc {current :: Stage, workdirSet :: Map Stage Bool}
  | Empty

rule :: Rule args
rule = customRule check (emptyState Empty)
  where
    code = "DL3045"
    severity = DLWarningC
    message = "`COPY` to a relative destination without `WORKDIR` set."

    check _ st (From from) = st |> modify (rememberStage from)
    check _ st (Workdir _) = st |> modify rememberWorkdir
    check line st (Copy (CopyArgs _ (TargetPath dest) _ _))
      | Acc s m <- state st, Just True <- Map.lookup s m = st -- workdir has been set
      | "/" `Text.isPrefixOf` Text.dropAround quotePredicate dest = st -- absolute dest. normal
      | ":\\" `Text.isPrefixOf` Text.drop 1 (Text.dropAround quotePredicate dest) = st -- absolute dest. windows
      | ":/" `Text.isPrefixOf` Text.drop 1 (Text.dropAround quotePredicate dest) = st -- absolute dest. windows
      | "$" `Text.isPrefixOf` Text.dropAround quotePredicate dest = st -- dest is a variable
      | otherwise = st |> addFail CheckFailure {..}
    check _ st _ = st
{-# INLINEABLE rule #-}

rememberStage :: BaseImage -> Acc -> Acc
rememberStage BaseImage {alias = Just als} Empty =
  Acc
    { current = Stage {stage = unImageAlias als},
      workdirSet = mempty
    }
rememberStage BaseImage {alias = Nothing, image} Empty =
  Acc
    { current = Stage {stage = imageName image},
      workdirSet = mempty
    }
rememberStage BaseImage {alias = Just als, image} Acc {..} =
  Acc
    { current = Stage {stage = unImageAlias als},
      workdirSet =
        let parentValue =
              Map.lookup (Stage { stage = imageName image}) workdirSet
                |> fromMaybe False
         in workdirSet
              |> Map.insert (Stage {stage = unImageAlias als}) parentValue
    }
rememberStage BaseImage {alias = Nothing, image} Acc {..} =
  Acc
    { current = Stage {stage = imageName image},
      workdirSet =
        let parentValue =
              Map.lookup (Stage {stage = imageName image}) workdirSet
                |> fromMaybe False
         in workdirSet
              |> Map.insert (Stage {stage = imageName image}) parentValue
    }

rememberWorkdir :: Acc -> Acc
rememberWorkdir Empty = Acc {current = None, workdirSet = Map.insert None True Map.empty}
rememberWorkdir Acc {..} = Acc {current, workdirSet = Map.insert current True workdirSet}

quotePredicate :: Char -> Bool
quotePredicate c
  | c == '"' = True
  | c == '\'' = True
  | otherwise = False
