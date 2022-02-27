module Hadolint.Config.Environment
  ( getConfigFromEnvironment
  )
where

import Data.Char (toLower)
import Data.Coerce (coerce)
import Data.Map (empty, fromList)
import Data.Set (Set, empty, fromList)
import Data.Text (Text, pack, unpack, drop, splitOn, breakOn)
import Hadolint.Formatter.Format (OutputFormat (..), readMaybeOutputFormat)
import Hadolint.Config.Configuration
import Hadolint.Rule
import Language.Docker.Syntax
import System.Environment


getConfigFromEnvironment :: IO PartialConfiguration
getConfigFromEnvironment =
  PartialConfiguration
    <$> maybeTruthy "HADOLINT_NOFAIL"
    <*> isSet "NO_COLOR"
    <*> maybeTruthy "HADOLINT_VERBOSE"
    <*> getFormat
    <*> getOverrideList "HADOLINT_OVERRIDE_ERROR"
    <*> getOverrideList "HADOLINT_OVERRIDE_WARNING"
    <*> getOverrideList "HADOLINT_OVERRIDE_INFO"
    <*> getOverrideList "HADOLINT_OVERRIDE_STYLE"
    <*> getOverrideList "HADOLINT_IGNORE"
    <*> getAllowedSet "HADOLINT_TRUSTED_REGISTRIES"
    <*> getLabelSchema "HADOLINT_REQUIRE_LABELS"
    <*> maybeTruthy "HADOLINT_STRICT_LABELS"
    <*> maybeTruthy "HADOLINT_DISABLE_IGNORE_PRAGMA"
    <*> getFailureThreshold


isSet :: String -> IO (Maybe Bool)
isSet name = do
  e <- lookupEnv name
  case e of
    Just _ -> return $ Just True
    Nothing -> return Nothing

maybeTruthy :: String -> IO (Maybe Bool)
maybeTruthy name = do
  e <- lookupEnv name
  case e of
    Just v ->
      if truthy v
      then return $ Just True
      else return $ Just False
    Nothing -> return Nothing

truthy :: String -> Bool
truthy s = map toLower s `elem` ["1", "y", "on", "true", "yes"]

getFormat :: IO (Maybe OutputFormat)
getFormat = do
  fmt <- lookupEnv "HADOLINT_FORMAT"
  return $ (readMaybeOutputFormat . pack) =<< fmt

getOverrideList :: String -> IO [RuleCode]
getOverrideList env = do
  maybeString <- lookupEnv env
  case maybeString of
    Just s -> return $ getRulecodes (pack s)
    Nothing -> return []

getRulecodes :: Text -> [RuleCode]
getRulecodes s = do
  list <- splitOn "," s
  let rules = coerce (list :: Text)
  return rules

getAllowedSet :: String -> IO (Set Registry)
getAllowedSet env = do
  maybeString <- lookupEnv env
  case maybeString of
    Just s -> return $ Data.Set.fromList (getAllowed (pack s))
    Nothing -> return Data.Set.empty

getAllowed :: Text -> [Registry]
getAllowed s = do
  list <- splitOn "," s
  let regs = coerce (list :: Text)
  return regs

getLabelSchema :: String -> IO LabelSchema
getLabelSchema env = do
  maybeString <- lookupEnv env
  case maybeString of
    Just s -> return $ Data.Map.fromList (labelSchemaFromText (pack s))
    Nothing -> return Data.Map.empty

labelSchemaFromText :: Text -> [(LabelName, LabelType)]
labelSchemaFromText txt =
  [ (ln, lt) | Right (ln, lt) <- map convertToLabelSchema (convertToPairs txt) ]

convertToPairs :: Text -> [(Text, Text)]
convertToPairs txt = map (breakOn ":") (splitOn "," txt)

convertToLabelSchema :: (Text, Text) -> Either String (LabelName, LabelType)
convertToLabelSchema (tln, tlt) =
  case (readEitherLabelType . Data.Text.drop 1) tlt of
    Right lt -> Right (coerce tln :: Text, lt)
    Left e -> Left (unpack e)

getFailureThreshold :: IO (Maybe DLSeverity)
getFailureThreshold = do
  ft <- lookupEnv "HADOLINT_FAILURE_THRESHOLD"
  return $ (readMaybeSeverity . pack) =<< ft
