module Hadolint.Rule.DL3057 (rule) where

import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import Hadolint.Rule
import Language.Docker.Syntax


data StageID = StageID
  { src :: Text.Text,
    name :: Text.Text,
    line :: Linenumber
  } deriving (Show, Eq, Ord)

data Acc
  = Acc StageID (Set.Set StageID) (Set.Set StageID)
  | Empty
  deriving (Show)

rule :: Rule args
rule = veryCustomRule check (emptyState Empty) markFailures
  where
    code = "DL3057"
    severity = DLIgnoreC
    message = "`HEALTHCHECK` instruction missing."

    check line state (From BaseImage {image, alias = Just als}) =
      state |> modify
                  ( currentStage
                      (imageName image)
                      (StageID (imageName image) (unImageAlias als) line)
                  )
    check line state (From BaseImage {image, alias = Nothing}) =
      state |> modify
                  ( currentStage
                      (imageName image)
                      (StageID (imageName image) (imageName image) line)
                  )
    check _ state (Healthcheck _) = state |> modify goodStage
    check _ state _ = state

    markFailures :: State Acc -> Failures
    markFailures (State fails (Acc _ _ b)) = Set.foldl' (Seq.|>) fails (Set.map makeFail b)
    markFailures st = failures st
    makeFail (StageID _ _ line) = CheckFailure {..}
{-# INLINEABLE rule #-}

currentStage :: Text.Text -> StageID -> Acc -> Acc
currentStage src stageid (Acc _ g b)
    | not $ Set.null (Set.filter (predicate src) g) = Acc stageid (g |> Set.insert stageid) b
    | otherwise = Acc stageid g (b |> Set.insert stageid)
  where
    predicate n0 StageID {name = n1} = n1 == n0
currentStage _ stageid Empty = Acc stageid Set.empty (Set.singleton stageid)

goodStage :: Acc -> Acc
goodStage (Acc stageid g b) = do
  let nowGood = recurseGood b stageid
  let good =
        g
          |> Set.union nowGood
          |> Set.insert stageid
      bad =
        b
          |> flip Set.difference nowGood
          |> Set.delete stageid
   in Acc
        stageid
        good
        bad
  where
    predicate StageID { src = s1 } StageID { name = n1 } = n1 == s1

    recurseGood :: Set.Set StageID -> StageID -> Set.Set StageID
    recurseGood bad sid = do
      let g1 = Set.filter (predicate sid) bad  -- bad stages to be marked good
          b1 = Set.difference bad g1  -- bad stages not to be marked good
       in if Set.null g1
            then g1
            else Set.union g1 $ Set.unions $ Set.map (recurseGood b1) g1

goodStage Empty = Empty
