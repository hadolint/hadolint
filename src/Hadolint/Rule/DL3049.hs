module Hadolint.Rule.DL3049 (rule) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import Hadolint.Rule
import Language.Docker.Syntax


rule :: LabelSchema -> Rule args
rule labelschema = mconcat $ fmap missingLabelRule (Map.keys labelschema)
{-# INLINEABLE rule #-}


data StageID = StageID
  { name :: BaseImage,
    line :: Linenumber
  } deriving (Eq, Ord, Show)

data Acc
  = Acc StageID (Set.Set StageID) (Set.Set StageID) (Set.Set StageID)
  | Empty
  deriving (Show)


-- missingLabelRule
--
-- triggers on a `FROM` instruction when label `label` is not defined within
-- that stage. Tracks defined labels through multi stage builds
missingLabelRule :: LabelName -> Rule args
missingLabelRule label = veryCustomRule check (emptyState Empty) markFailure
  where
    code = "DL3049"
    severity = DLInfoC
    message = "Label `" <> label <> "` is missing."
    check line state (From img) =
      state |> modify (currentStage (StageID img line))
    check _ state (Copy (CopyArgs _ _) (CopyFlags _ _ _ (CopySource src))) =
      state |> modify (markSilentByAlias src)
    check _ state (Label pairs)
      | label `elem` fmap fst pairs =
          state
            |> modify (markSilentByAlias (getCurrentStageName state))
            |> modify markGood
      | otherwise = state
    check _ state _ = state

    markFailure :: State Acc -> Failures
    markFailure (State fails (Acc _ _ _ b)) = Set.foldl' (Seq.|>) fails (Set.map markFail b)
    markFailure st = failures st

    markFail (StageID _ line) = CheckFailure {..}


currentStage :: StageID -> Acc -> Acc
currentStage stageid Empty = Acc stageid Set.empty Set.empty (Set.singleton stageid)
currentStage stageid (Acc _ g s b)
  | not $ Set.null (Set.filter (predicate stageid) g) =
      Acc stageid (g |> Set.insert stageid) s b
  | otherwise = Acc stageid g s (b |> Set.insert stageid)
  where
    predicate (StageID _ _) (StageID BaseImage {alias = Nothing} _) = False
    predicate (StageID BaseImage {image} _) (StageID BaseImage {alias = Just als} _) =
      unImageAlias als == imageName image

markGood :: Acc -> Acc
markGood Empty = Empty
markGood (Acc stageid good silent bad) =
  Acc stageid (good |> Set.insert stageid) (silent |> Set.delete stageid) (bad |> Set.delete stageid)

markSilentByAlias :: Text.Text -> Acc -> Acc
markSilentByAlias _ Empty = Empty
markSilentByAlias silentname (Acc stageid good silent bad) =
  Acc stageid good (silent |> Set.union stages) (bad |> remove stages)
  where
    stages = Set.filter byName bad
    byName (StageID BaseImage {alias = Nothing} _) = False
    byName (StageID BaseImage {alias = Just als} _) = unImageAlias als == silentname
    remove set fromThis = Set.difference fromThis set

getCurrentStageName :: State Acc -> Text.Text
getCurrentStageName (State _ (Acc (StageID BaseImage {image, alias = Nothing} _) _ _ _)) = imageName image
getCurrentStageName (State _ (Acc (StageID BaseImage {alias = Just als} _) _ _ _)) = unImageAlias als
getCurrentStageName _ = ""
