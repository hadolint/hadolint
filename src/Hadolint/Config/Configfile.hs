module Hadolint.Config.Configfile
  ( getConfigFromFile
  )
where

import Control.Monad (when, filterM)
import Data.Maybe (listToMaybe)
import Data.YAML as Yaml
import qualified Data.ByteString as Bytes
import Hadolint.Config.Configuration (PartialConfiguration (..))
import System.Directory
  ( XdgDirectory (..),
    doesFileExist,
    getCurrentDirectory,
    getAppUserDataDirectory,
    getUserDocumentsDirectory,
    getXdgDirectory,
  )
import System.FilePath ((</>))
import System.IO (hPrint, stderr)


getConfigFromFile ::
  Maybe FilePath -> Bool -> IO (Either String PartialConfiguration)
getConfigFromFile maybeExplicitPath verbose = do
  maybePath <- getConfig maybeExplicitPath
  when verbose $ hPrint stderr $ getFilePathDescription maybePath
  case maybePath of
    Nothing -> return $ Right mempty
    Just path -> readConfig path

readConfig :: FilePath -> IO (Either String PartialConfiguration)
readConfig path = do
  contents <- Bytes.readFile path
  return $ case Yaml.decode1Strict contents of
    Left (_, err) -> Left (formatError err path)
    Right config -> Right config

getFilePathDescription :: Maybe FilePath -> String
getFilePathDescription Nothing =
  "No configuration was specified. Using default configuration"
getFilePathDescription (Just filepath) = "Configuration file used: " ++ filepath

formatError :: String -> String -> String
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
      "The key 'trustedRegistries' should contain the names of the allowed",
      "docker registries:\n",
      "trustedRegistries:",
      "\t- docker.io",
      "\t- my-company.com",
      "",
      err
    ]

-- | Gets the configuration file which Hadolint uses
getConfig :: Maybe FilePath -> IO (Maybe FilePath)
getConfig maybeConfig =
  case maybeConfig of
    Nothing -> findConfig
    _ -> return maybeConfig

-- | If no configuration file path was given on the command line, Hadolint
-- searches these locations or their equivalents on MacOS/Windows:
--  - $(pwd)/.hadolint.{yaml|yml}
--  - $HOME/.config/hadolint.{yaml|yml}
--  - $HOME/.hadolint/{hadolint|config}.{yaml|yml}
--  - $HOME/.hadolint.{yaml|yml}
-- The first file found is used, all other are ignored.
findConfig :: IO (Maybe FilePath)
findConfig = do
  filesInCWD <- traverse
                  (\filePath -> (</> filePath) <$> getCurrentDirectory)
                  hiddenConfigs
  filesInXdgConfig <- traverse (getXdgDirectory XdgConfig) visibleConfigs
  filesInAppData <- traverse
                      (\fp -> (</> fp) <$> getAppUserDataDirectory "hadolint")
                      (visibleConfigs <> moreConfigs)
  filesInHome <- traverse
                   (\fp -> (</> fp) <$> getUserDocumentsDirectory)
                   hiddenConfigs
  listToMaybe
    <$> filterM
          doesFileExist
          (filesInCWD <> filesInXdgConfig <> filesInAppData <> filesInHome)
  where
    hiddenConfigs = [".hadolint.yaml", ".hadolint.yml"]
    visibleConfigs = ["hadolint.yaml", "hadolint.yml"]
    moreConfigs = ["config.yaml", "config.yml"]
