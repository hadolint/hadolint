module Hadolint.Rule.DL3060 (rule) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Hadolint.Rule
import qualified Hadolint.Shell as Shell
import Language.Docker.Syntax


data Acc
  = Acc
      { current :: BaseImage,
        active :: Map.Map Text.Text Linenumber,
        inactive :: Map.Map Linenumber BaseImage
      }
  | Empty
  deriving (Show)


rule :: Rule Shell.ParsedShell
rule = dl3060 <> onbuild dl3060
{-# INLINEABLE rule #-}

dl3060 :: Rule Shell.ParsedShell
dl3060 = veryCustomRule check (emptyState Empty) markFailures
  where
    code = "DL3060"
    severity = DLInfoC
    message = "`yarn cache clean` missing after `yarn install` was run."

    check line st (From from) =
      st |> modify (rememberStage line from)
    check line st (Run (RunArgs args _))
      | foldArguments (Shell.anyCommands yarnInstall) args
          && foldArguments (Shell.noCommands yarnCacheClean) args =
        st |> modify (rememberLine line)
      | otherwise = st
    check _ st _ = st

    -- Produce failures from the final state Acc
    markFailures :: State Acc -> Failures
    markFailures (State fails Empty) = fails
    markFailures (State _ Acc {..}) = inactive |> Map.foldMapWithKey mapFail
      where
        mapFail line from
          | from == current = pure CheckFailure {..}
          | BaseImage {alias = Just (ImageAlias als)} <- from,
            Just _ <- Map.lookup als active = pure CheckFailure {..}
          | otherwise = mempty
{-# INLINEABLE dl3060 #-}

rememberStage :: Linenumber -> BaseImage -> Acc -> Acc
rememberStage line stage@BaseImage {image = Image _ als} Empty =
  Acc stage (Map.singleton als line) Map.empty
rememberStage line stage@BaseImage {image = Image _ als} (Acc _ stages o) =
  Acc stage (Map.insert als line stages) o

rememberLine :: Linenumber -> Acc -> Acc
rememberLine line Empty = Acc scratch mempty (Map.singleton line scratch)
rememberLine line (Acc from stages o) = Acc from stages (Map.insert line from o)

yarnInstall :: Shell.Command -> Bool
yarnInstall = Shell.cmdHasArgs "yarn" ["install"]

yarnCacheClean :: Shell.Command -> Bool
yarnCacheClean = Shell.cmdHasArgs "yarn" ["cache", "clean"]

-- | This is needed as placeholder when no FROM statement has yet been
-- encountered.
scratch :: BaseImage
scratch =
  ( BaseImage
      { image = "scratch",
        tag = Nothing,
        digest = Nothing,
        alias = Nothing,
        platform = Nothing
      }
  )
