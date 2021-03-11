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
  { name :: Text.Text,
    line :: Linenumber
  } deriving (Eq, Ord, Show)

data Acc
  = Acc StageID (Set.Set StageID) (Set.Set StageID)
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
    check line state (From BaseImage {image, alias = Just als}) =
        state |> modify (currentStage (imageName image) (StageID (unImageAlias als) line))
    check line state (From BaseImage {image, alias = Nothing}) =
        state |> modify (currentStage (imageName image) (StageID (imageName image) line))
    check _ state (Label pairs)
        | label `elem` fmap fst pairs = state |> modify goodStage
        | otherwise = state
    check _ state _ = state

    markFailure :: State Acc -> Failures
    markFailure (State fails (Acc _ _ b)) = Set.foldl' (Seq.|>) fails (Set.map markFail b)
    markFailure st = failures st

    markFail (StageID _ line) = CheckFailure {..}

currentStage :: Text.Text -> StageID -> Acc -> Acc
currentStage src stageid (Acc _ g b)
    | not $ Set.null (Set.filter (predicate src) g) = Acc stageid (g |> Set.insert stageid) b
    | otherwise = Acc stageid g (b |> Set.insert stageid)
  where
    predicate n0 StageID {name = n1} = n1 == n0
currentStage _ stageid Empty = Acc stageid Set.empty (Set.singleton stageid)

goodStage :: Acc -> Acc
goodStage (Acc stageid g b) = Acc stageid (g |> Set.insert stageid) (b |> Set.delete stageid)
goodStage Empty = Empty
