module Hadolint.Rule.DL3009 (rule) where

import Hadolint.Rule
import Language.Docker.Syntax
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Hadolint.Shell as Shell


data Acc
  = Acc
      { lastFrom :: BaseImage,
        dockerClean :: Bool,
        stages :: Map.Map Text.Text Linenumber,
        forgets :: Map.Map Linenumber BaseImage
      }
  | Empty
  deriving (Eq, Show)


rule :: Rule Shell.ParsedShell
rule = dl3009 <> onbuild dl3009
{-# INLINEABLE rule #-}

dl3009 :: Rule Shell.ParsedShell
dl3009 = veryCustomRule check (emptyState Empty) markFailures
  where
    code = "DL3009"
    severity = DLInfoC
    message = "Delete the apt-get lists after installing something"

    check line st (From from) = st |> modify (rememberStage line from)
    check line st (Run (RunArgs args flags))
      | not (foldArguments forgotToCleanup args) =
          if foldArguments disabledDockerClean args
          then st |> modify rememberDockerClean
          else st
      | hasCacheDirectory "/var/lib/apt/lists" flags = st
      | hasCacheDirectory "/var/lib/apt" flags
        && hasCacheDirectory "/var/cache/apt" flags = st
      | otherwise = st |> modify (rememberLine line)
    check _ st _ = st

    -- Convert the final state into failures
    markFailures (State fails Empty) = fails
    markFailures (State _ Acc {..}) = forgets |> Map.foldMapWithKey mapFail
      where
        mapFail line from
          | from == lastFrom = pure CheckFailure {..}
          | BaseImage {alias = Just (ImageAlias als)} <- from, -- Check if this stage is used later
            Just _ <- Map.lookup als stages =
            -- If the same alias is used in another stage, fail
            pure CheckFailure {..}
          | otherwise = mempty
{-# INLINEABLE dl3009 #-}

rememberStage :: Linenumber -> BaseImage -> Acc -> Acc
rememberStage line from@BaseImage {image = Image _ als} (Acc _ _ stages o) = Acc from True (Map.insert als line stages) o
rememberStage line from@BaseImage {image = Image _ als} Empty = Acc from True (Map.singleton als line) Map.empty

rememberLine :: Linenumber -> Acc -> Acc
rememberLine line (Acc from clean stages o) = Acc from clean stages (Map.insert line from o)
rememberLine line Empty = Acc emptyImage True mempty (Map.singleton line emptyImage)

rememberDockerClean :: Acc -> Acc
rememberDockerClean Empty = Acc emptyImage False mempty mempty
rememberDockerClean (Acc from _ stages forget) = Acc from False stages forget

forgotToCleanup :: Shell.ParsedShell -> Bool
forgotToCleanup args
  | hasUpdate args && not hasCleanup = True
  | otherwise = False
  where
    hasCleanup =
      any (Shell.cmdHasArgs "rm" ["-rf", "/var/lib/apt/lists/*"]) (Shell.presentCommands args)

hasUpdate :: Shell.ParsedShell -> Bool
hasUpdate args =
  any (Shell.cmdHasArgs "apt-get" ["update"]) (Shell.presentCommands args)

disabledDockerClean :: Shell.ParsedShell -> Bool
disabledDockerClean args
  | removesScript || keepsPackages = True
  | otherwise = False
  where
    removesScript =
      any
        (Shell.cmdHasArgs "rm" ["/etc/apt/apt.conf.d/docker-clean"])
        (Shell.presentCommands args)
    keepsPackages =
      any
        ( Shell.cmdHasArgs
            "echo"
            ["\'Binary::apt::APT::Keep-Downloaded-Packages \"true\";\'"]
        )
        (Shell.presentCommands args)

hasCacheDirectory :: Text.Text -> RunFlags -> Bool
hasCacheDirectory dir RunFlags { mount } =
  not ( null $ Set.filter (isCacheMount dir) mount)

isCacheMount :: Text.Text -> RunMount -> Bool
isCacheMount dir (CacheMount CacheOpts {cTarget = TargetPath {unTargetPath = t}}) = dir `Text.isPrefixOf` t
isCacheMount _ _ = False

-- | Even though dockerfiles without a FROM are not valid, we still want to provide some feedback for this rule
-- so we pretend there is a base image at the start of the file if there is none
emptyImage :: BaseImage
emptyImage = (BaseImage {image = "scratch", tag = Nothing, digest = Nothing, alias = Nothing, platform = Nothing})
