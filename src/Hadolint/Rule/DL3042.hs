module Hadolint.Rule.DL3042 (rule) where

import Data.List (isInfixOf)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isNothing, isJust, fromJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Hadolint.Rule
import qualified Hadolint.Shell as Shell
import Language.Docker.Syntax


-- This data encapsulates the name of a build stage. It may be None withing an
-- `ONBUILD` context.
data Stage
  = Stage {stage :: Text}
  | None
  deriving (Eq, Ord)

data Acc
  = Acc {current :: Stage, noCacheMap :: Map Stage Bool}
  | Empty


rule :: Rule Shell.ParsedShell
rule = dl3042 <> onbuild dl3042
{-# INLINEABLE rule #-}

dl3042 :: Rule Shell.ParsedShell
dl3042 = customRule check (emptyState Empty)
  where
    code = "DL3042"
    severity = DLWarningC
    message = "Avoid use of cache directory with pip.\
              \ Use `pip install --no-cache-dir <package>`"
    check _ st (From from) = st |> modify (rememberStage from)
    check _ st (Env pairs) = st |> modify (registerEnv pairs)
    check line st (Run (RunArgs args _))
      | Acc s ncm <- state st, Just True <- Map.lookup s ncm = st
      | foldArguments pipNoCacheDirIsSet args = st
      | foldArguments (Shell.noCommands forgotNoCacheDir) args = st
      | otherwise = st |> addFail CheckFailure {..}
    check _ st _ = st
{-# INLINEABLE dl3042 #-}

forgotNoCacheDir :: Shell.Command -> Bool
forgotNoCacheDir cmd =  Shell.isPipInstall cmd
    && not (usesNoCacheDir cmd)
    && not (isPipWrapper cmd)

usesNoCacheDir :: Shell.Command -> Bool
usesNoCacheDir cmd = "--no-cache-dir" `elem` Shell.getArgs cmd

pipNoCacheDirIsSet :: Shell.ParsedShell -> Bool
pipNoCacheDirIsSet shell = any
  (`Text.isPrefixOf` Text.drop 1
    (Text.dropWhile (/= '=')
      (snd
        (Text.breakOn "PIP_NO_CACHE_DIR=" (Shell.original shell)
        )
      )
    )
  ) truthy

isPipWrapper :: Shell.Command -> Bool
isPipWrapper cmd@(Shell.Command name _ _) = isWrapper "pipx" || isWrapper "pipenv"
  where
    isWrapper :: Text.Text -> Bool
    isWrapper w =
      w `Text.isInfixOf` name
        || ("python" `Text.isPrefixOf` name && ["-m", w] `isInfixOf` Shell.getArgs cmd)

rememberStage :: BaseImage -> Acc -> Acc
rememberStage BaseImage {alias = Just als} Empty =
  Acc
    { current = Stage {stage = unImageAlias als},
      noCacheMap = mempty
    }
rememberStage BaseImage {alias = Nothing, image} Empty =
  Acc
    { current = Stage {stage = imageName image},
      noCacheMap = mempty
    }
rememberStage BaseImage {alias = Just als, image} Acc {..} =
  Acc
    { current = Stage {stage = unImageAlias als},
      noCacheMap =
        let parentValue =
              Map.lookup (Stage {stage = imageName image}) noCacheMap |> fromMaybe False
         in noCacheMap |> Map.insert (Stage {stage = unImageAlias als}) parentValue
    }
rememberStage BaseImage {alias = Nothing, image} Acc {..} =
  Acc
    { current = Stage {stage = imageName image},
      noCacheMap =
        let parentValue =
              Map.lookup (Stage {stage = imageName image}) noCacheMap |> fromMaybe False
         in noCacheMap |> Map.insert (Stage {stage = imageName image}) parentValue
    }

registerEnv :: Pairs -> Acc -> Acc
registerEnv pairs Empty
  | pipNoCacheDirSet pairs =
    Acc {current = None, noCacheMap = Map.insert None True Map.empty}
  | otherwise = Empty
registerEnv pairs Acc {..}
  | pipNoCacheDirSet pairs =
    Acc {current, noCacheMap = Map.insert current True noCacheMap}
  | otherwise = Acc {..}

pipNoCacheDirSet :: Pairs -> Bool
pipNoCacheDirSet [] = False
pipNoCacheDirSet pairs
  | isNothing (lookup "PIP_NO_CACHE_DIR" pairs) = False
  | val <- lookup "PIP_NO_CACHE_DIR" pairs,
    isJust val && fromJust val `notElem` truthy = False
  | otherwise = True

truthy :: Set Text
truthy = Set.fromList ["1", "true", "True", "TRUE", "on", "On", "ON", "yes", "Yes", "YES"]
