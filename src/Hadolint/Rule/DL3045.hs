module Hadolint.Rule.DL3045 (rule) where

import qualified Data.Map as Map
import qualified Data.Text as Text
import Hadolint.Rule
import Language.Docker.Syntax

data Acc
  = Acc Text.Text (Map.Map Text.Text Bool)
  | Empty

-- The state here keeps the image name/alias of the current build stage
-- and a map from image names/aliases to a Bool, saving whether or
-- not a `WORKDIR` has been set in a build stage.
rule :: Rule args
rule = customRule check (emptyState Empty)
  where
    code = "DL3045"
    severity = DLWarningC
    message = "`COPY` to a relative destination without `WORKDIR` set."

    check _ st (From from) = st |> modify (rememberStage from)
    check _ st (Workdir _) = st |> modify rememberWorkdir
    check line st (Copy (CopyArgs _ (TargetPath dest) _ _))
      | Acc s m <- state st, Map.member s m = st -- workdir has been set
      | "/" `Text.isPrefixOf` Text.dropAround quotePredicate dest = st -- absolute dest. normal
      | ":\\" `Text.isPrefixOf` Text.drop 1 (Text.dropAround quotePredicate dest) = st -- absolute dest. windows
      | "$" `Text.isPrefixOf` Text.dropAround quotePredicate dest = st -- dest is a variable
      | otherwise = st |> addFail CheckFailure {..}
    check _ st _ = st

rememberStage :: BaseImage -> Acc -> Acc
rememberStage BaseImage {alias = Just als, image} Empty =
  Acc
    (unImageAlias als)
    (Map.singleton (imageName image) False)
rememberStage BaseImage {alias = Nothing, image} Empty =
  Acc
    (imageName image)
    (Map.singleton (imageName image) False)
rememberStage BaseImage {alias = Just als, image} (Acc _ m) =
  Acc
    (unImageAlias als)
    (Map.union m (Map.singleton (imageName image) False))
rememberStage BaseImage {alias = Nothing, image} (Acc _ m) = Acc (imageName image) m

rememberWorkdir :: Acc -> Acc
rememberWorkdir Empty = Empty
rememberWorkdir (Acc stage m) = Acc stage (Map.insert stage True m)

quotePredicate :: Char -> Bool
quotePredicate c
  | c == '"' = True
  | c == '\'' = True
  | otherwise = False
