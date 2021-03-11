{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Hadolint.Config
  ( applyConfig,
    ConfigFile (..),
    OverrideConfig (..)
  ) where

import qualified Control.Foldl.Text as Text
import Control.Monad (filterM)
import qualified Data.ByteString as Bytes
import Data.Coerce (coerce)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Map as Map
import qualified Data.Set as Set
import Data.YAML ((.:?))
import qualified Data.YAML as Yaml
import GHC.Generics (Generic)
import qualified Hadolint.Lint as Lint
import qualified Hadolint.Process as Process
import qualified Hadolint.Rule
import qualified Language.Docker as Docker
import System.Directory
  ( XdgDirectory (..),
    doesFileExist,
    getCurrentDirectory,
    getXdgDirectory,
  )
import System.FilePath ((</>))

data OverrideConfig = OverrideConfig
  { overrideErrorRules :: Maybe [Lint.ErrorRule],
    overrideWarningRules :: Maybe [Lint.WarningRule],
    overrideInfoRules :: Maybe [Lint.InfoRule],
    overrideStyleRules :: Maybe [Lint.StyleRule]
  }
  deriving (Show, Eq, Generic)

data ConfigFile = ConfigFile
  { overrideRules :: Maybe OverrideConfig,
    ignoredRules :: Maybe [Lint.IgnoreRule],
    trustedRegistries :: Maybe [Lint.TrustedRegistry],
    labelSchemaConfig :: Maybe Hadolint.Rule.LabelSchema,
    strictLabelSchema :: Maybe Bool
  }
  deriving (Show, Eq, Generic)

instance Yaml.FromYAML OverrideConfig where
  parseYAML = Yaml.withMap "OverrideConfig" $ \m ->
    OverrideConfig
      <$> m .:? "error"
      <*> m .:? "warning"
      <*> m .:? "info"
      <*> m .:? "style"

instance Yaml.FromYAML ConfigFile where
  parseYAML = Yaml.withMap "ConfigFile" $ \m -> do
    overrideRules <- m .:? "override"
    ignored <- m .:? "ignored"
    let ignoredRules = coerce (ignored :: Maybe [Text.Text])
    trustedRegistries <- m .:? "trustedRegistries"
    labelSchemaConfig <- m .:? "label-schema"
    strictLabelSchema <- m .:? "strict-labels"
    return ConfigFile {..}

-- | If both the ignoreRules and rulesConfig properties of Lint options are empty
-- then this function will fill them with the default found in the passed config
-- file. If there is an error parsing the default config file, this function will
-- return the error string.
applyConfig :: Maybe FilePath -> Lint.LintOptions -> IO (Either String Lint.LintOptions)
applyConfig maybeConfig o
  | not (Prelude.null (Lint.ignoreRules o)) && Lint.rulesConfig o /= mempty = return (Right o)
  | otherwise = do
    theConfig <-
      case maybeConfig of
        Nothing -> findConfig
        c -> return c
    case theConfig of
      Nothing -> return (Right o)
      Just config -> parseAndApply config
  where
    findConfig = do
      localConfigFile <- (</> ".hadolint.yaml") <$> getCurrentDirectory
      configFile <- getXdgDirectory XdgConfig "hadolint.yaml"
      listToMaybe <$> filterM doesFileExist [localConfigFile, configFile]

    parseAndApply :: FilePath -> IO (Either String Lint.LintOptions)
    parseAndApply configFile = do
      contents <- Bytes.readFile configFile
      case Yaml.decode1Strict contents of
        Left (_, err) -> return $ Left (formatError err configFile)
        Right (ConfigFile Nothing ignore trusted labelschema strictlabels) ->
          return $
            Right (applyOverride Nothing Nothing Nothing Nothing ignore trusted labelschema strictlabels)
        Right (ConfigFile (Just (OverrideConfig errors warnings infos styles)) ignore trusted labelschema strictlabels) ->
          return $
            Right (applyOverride errors warnings infos styles ignore trusted labelschema strictlabels)

    applyOverride errors warnings infos styles ignore trusted labelschema strictlabels =
      applyRulesConfig trusted labelschema strictlabels
        . applyIgnore ignore
        . applyStyles styles
        . applyInfos infos
        . applyWarnings warnings
        . applyErrors errors
        $ o

    applyErrors errors opts =
      case Lint.errorRules opts of
        [] -> opts {Lint.errorRules = fromMaybe [] errors}
        _ -> opts

    applyWarnings warnings opts =
      case Lint.warningRules opts of
        [] -> opts {Lint.warningRules = fromMaybe [] warnings}
        _ -> opts

    applyInfos infos opts =
      case Lint.infoRules opts of
        [] -> opts {Lint.infoRules = fromMaybe [] infos}
        _ -> opts

    applyStyles styles opts =
      case Lint.styleRules opts of
        [] -> opts {Lint.styleRules = fromMaybe [] styles}
        _ -> opts

    applyIgnore ignore opts =
      case Lint.ignoreRules opts of
        [] -> opts {Lint.ignoreRules = fromMaybe [] ignore}
        _ -> opts

    applyRulesConfig trusted labelschema strictlabels opts =
      opts { Lint.rulesConfig =
        ((`applyLabelSchema` labelschema) .
        (`applyStrictLabels` strictlabels) .
        (`applyTrustedRegistries` trusted)) (Lint.rulesConfig opts) }

    applyStrictLabels :: Process.RulesConfig -> Maybe Bool -> Process.RulesConfig
    applyStrictLabels rc (Just strict) =
        rc { Process.strictLabels = Process.strictLabels rc || strict }
    applyStrictLabels rc _ = rc

    applyLabelSchema :: Process.RulesConfig -> Maybe Hadolint.Rule.LabelSchema -> Process.RulesConfig
    applyLabelSchema rc (Just labelschema)
        | Map.null (Process.labelSchema rc) =
            rc { Process.labelSchema = labelschema}
        | otherwise = rc
    applyLabelSchema rc _ = rc

    applyTrustedRegistries :: Process.RulesConfig -> Maybe [Lint.TrustedRegistry] -> Process.RulesConfig
    applyTrustedRegistries rc (Just trusted)
        | Prelude.null (Process.allowedRegistries rc) =
            rc { Process.allowedRegistries = Set.fromList . coerce $ trusted }
        | otherwise = rc
    applyTrustedRegistries rc _ = rc


    formatError err config =
      unlines
        [ "Error parsing your config file in  '" ++ config ++ "':",
          "It should contain one of the keys 'override', 'ignored'",
          "or 'trustedRegistries'. For example:\n",
          "ignored:",
          "\t- DL3000",
          "\t- SC1099\n\n",
          "The key 'override' should contain only lists with names 'error',",
          "'warning', 'info' or 'style', which each name rules to override the",
          "severity on. For example:\n",
          "override:",
          "\terror:",
          "\t\t- DL3008\n\n",
          "The key 'trustedRegistries' should contain the names of the allowed docker registries:\n",
          "allowedRegistries:",
          "\t- docker.io",
          "\t- my-company.com",
          "",
          err
        ]
