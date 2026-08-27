module Hadolint.Rule.DL3072 (rule) where

import qualified Data.Text as Text
import Hadolint.Rule
import qualified Hadolint.Shell as Shell
import qualified Hadolint.Utils as Utils
import Language.Docker.Syntax


rule :: Rule Shell.ParsedShell
rule = dl3072 <> onbuild dl3072
{-# INLINEABLE rule #-}

dl3072 :: Rule Shell.ParsedShell
dl3072 = simpleRule code severity message check
  where
    code = "DL3072"
    severity = DLInfoC
    message =
      "Use BuildKit cache mount for Gradle (`--mount=type=cache,target=/root/.gradle`) \
      \-- without it, the Gradle cache is baked into the image layer and bloats the image"

    check (Run (RunArgs args flags))
      | foldArguments (Shell.noCommands isGradleCommand) args = True
      | Utils.hasCacheOrTmpfsMountWith ".gradle" flags = True
      | otherwise = False
    check _ = True
{-# INLINEABLE dl3072 #-}

isGradleCommand :: Shell.Command -> Bool
isGradleCommand (Shell.Command n _ _) =
  n `elem` ["gradle", "gradlew"] || "/gradlew" `Text.isSuffixOf` n
