module Hadolint.Config
  ( applyConfig,
    ConfigFile (..),
    OverrideConfig (..),
  )
where

import Control.Applicative ((<|>))
import qualified Control.Foldl.Text as Text
import Control.Monad (filterM)
import qualified Data.ByteString as Bytes
import Data.Coerce (coerce)
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Set as Set
import Data.YAML ((.:?))
import qualified Data.YAML as Yaml
import GHC.Generics (Generic)
import qualified Hadolint.Lint as Lint
import qualified Hadolint.Process as Process
import qualified Hadolint.Rule as Rule
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

instance Semigroup OverrideConfig where
  OverrideConfig a1 a2 a3 a4 <> OverrideConfig b1 b2 b3 b4 =
    OverrideConfig (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4)

instance Monoid OverrideConfig where
  mempty = OverrideConfig Nothing Nothing Nothing Nothing

data ConfigFile = ConfigFile
  { overrideRules :: Maybe OverrideConfig,
    ignoredRules :: Maybe [Lint.IgnoreRule],
    trustedRegistries :: Maybe [Lint.TrustedRegistry],
    labelSchemaConfig :: Maybe Rule.LabelSchema,
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
      return $ case Yaml.decode1Strict contents of
        Left (_, err) -> Left (formatError err configFile)
        Right config -> Right $ fromMaybe o (applyOverride config)

    applyOverride ConfigFile {..} =
      -- Maybe.do
      do
        OverrideConfig {..} <- overrideRules <|> Just mempty
        overrideError <- overrideErrorRules <|> Just mempty
        overrideWarning <- overrideWarningRules <|> Just mempty
        overrideInfo <- overrideInfoRules <|> Just mempty
        overrideStyle <- overrideStyleRules <|> Just mempty
        overrideIgnored <- ignoredRules <|> Just mempty

        trusted <- Set.fromList . coerce <$> (trustedRegistries <|> Just mempty)
        schema <- labelSchemaConfig <|> Just mempty
        strictLabels <- strictLabelSchema <|> Just False

        let rulesConfig = Lint.rulesConfig o

        return $
          Lint.LintOptions
            { Lint.errorRules = Lint.errorRules o <|> overrideError,
              Lint.warningRules = Lint.warningRules o <|> overrideWarning,
              Lint.infoRules = Lint.infoRules o <|> overrideInfo,
              Lint.styleRules = Lint.styleRules o <|> overrideStyle,
              Lint.ignoreRules = Lint.ignoreRules o <|> overrideIgnored,
              Lint.rulesConfig =
                Process.RulesConfig
                  { Process.allowedRegistries = Process.allowedRegistries rulesConfig `ifNull` trusted,
                    Process.labelSchema = Process.labelSchema rulesConfig `ifNull` schema,
                    Process.strictLabels = Process.strictLabels rulesConfig || strictLabels
                  }
            }

    ifNull value override = if null value then override else value

    formatError err config =
      Prelude.unlines
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
