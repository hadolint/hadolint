{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Hadolint.Config (applyConfig, ConfigFile(..)) where

import Control.Monad (filterM)
import Data.Coerce (coerce)
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.ByteString as Bytes
import qualified Data.Set as Set
import qualified Data.YAML as Yaml
import Data.YAML ((.:?))
import GHC.Generics
import qualified Language.Docker as Docker
import System.Directory
       (XdgDirectory(..), doesFileExist, getCurrentDirectory,
        getXdgDirectory)
import System.FilePath ((</>))

import qualified Hadolint.Lint as Lint
import qualified Hadolint.Rules as Rules

data ConfigFile = ConfigFile
    { ignoredRules :: Maybe [Lint.IgnoreRule]
    , trustedRegistries :: Maybe [Lint.TrustedRegistry]
    } deriving (Show, Eq, Generic)

instance Yaml.FromYAML ConfigFile where
  parseYAML = Yaml.withMap "ConfigFile" $ \m -> ConfigFile
       <$> m .:? "ignored"
       <*> m .:? "trustedRegistries"

-- | If both the ignoreRules and rulesConfig properties of Lint options are empty
-- then this function will fill them with the default found in the passed config
-- file. If there is an error parsing the default config file, this function will
-- return the error string.
applyConfig :: Maybe FilePath -> Lint.LintOptions -> IO (Either String Lint.LintOptions)
applyConfig maybeConfig o
    | not (null (Lint.ignoreRules o)) && Lint.rulesConfig o /= mempty = return (Right o)
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
            Left err -> return $ Left (formatError err configFile)
            Right (ConfigFile ignore trusted) -> return (Right (override ignore trusted))
    -- | Applies the configuration found in the file to the passed Lint.LintOptions
    override ignore trusted = applyTrusted trusted . applyIgnore ignore $ o
    applyIgnore ignore opts =
        case Lint.ignoreRules opts of
            [] -> opts {Lint.ignoreRules = fromMaybe [] ignore}
            _ -> opts
    applyTrusted trusted opts
        | null (Rules.allowedRegistries (Lint.rulesConfig opts)) =
            opts {Lint.rulesConfig = toRules trusted <> Lint.rulesConfig opts}
        | otherwise = opts
    -- | Converts a list of TrustedRegistry to a RulesConfig record
    toRules (Just trusted) = Rules.RulesConfig (Set.fromList . coerce $ trusted)
    toRules _ = mempty
    formatError err config =
      unlines
          [ "Error parsing your config file in  '" ++ config ++ "':"
          , "It should contain one of the keys 'ignored' or 'trustedRegistries'. For example:\n"
          , "ignored:"
          , "\t- DL3000"
          , "\t- SC1099\n\n"
          , "The key 'trustedRegistries' should contain the names of the allowed docker registries:\n"
          , "allowedRegistries:"
          , "\t- docker.io"
          , "\t- my-company.com"
          , ""
          , err
          ]
