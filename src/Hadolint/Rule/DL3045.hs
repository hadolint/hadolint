module Hadolint.Rule.DL3045 (rule) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Hadolint.Rule
import Language.Docker.Syntax

-- | The state here keeps the image name/alias of the current build stage
-- and a map from image names/aliases to a Bool, saving whether or
-- not a `WORKDIR` has been set in a build stage.
data Acc
  = Acc {current :: Text, workdirSet :: Map Text Bool}
  | Empty
  deriving (Show)

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
      | "$" `Text.isPrefixOf` Text.dropAround quotePredicate dest = st -- dest is a variable
      | otherwise = st |> addFail CheckFailure {..}
    check _ st _ = st

rememberStage :: BaseImage -> Acc -> Acc
rememberStage BaseImage {alias = Just als} Empty =
  Acc
    { current = unImageAlias als,
      workdirSet = mempty
    }
rememberStage BaseImage {alias = Nothing, image} Empty =
  Acc
    { current = imageName image,
      workdirSet = mempty
    }
rememberStage BaseImage {alias = Just als, image} Acc {..} =
  Acc
    { current = unImageAlias als,
      workdirSet =
        let parentValue =
              Map.lookup (imageName image) workdirSet
                |> fromMaybe False
         in workdirSet
              |> Map.insert (unImageAlias als) parentValue
    }
rememberStage BaseImage {alias = Nothing, image} Acc {..} =
  Acc
    { current = imageName image,
      workdirSet =
        let parentValue =
              Map.lookup (imageName image) workdirSet
                |> fromMaybe False
         in workdirSet
              |> Map.insert (imageName image) parentValue
    }

rememberWorkdir :: Acc -> Acc
rememberWorkdir Empty = Empty
rememberWorkdir Acc {..} = Acc {current, workdirSet = Map.insert current True workdirSet}

quotePredicate :: Char -> Bool
quotePredicate c
  | c == '"' = True
  | c == '\'' = True
  | otherwise = False
