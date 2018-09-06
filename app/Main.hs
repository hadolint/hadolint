{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Control.Applicative
import Control.Monad (filterM)
import Data.Coerce (coerce)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Semigroup ((<>))
import qualified Data.Set as Set
import Data.String
import Data.Text (Text)
import qualified Data.Yaml as Yaml
import GHC.Generics
import qualified Language.Docker as Docker
import Language.Docker.Syntax (Dockerfile)
import Options.Applicative hiding (ParseError)
import System.Directory
       (XdgDirectory(..), doesFileExist, getCurrentDirectory,
        getXdgDirectory)
import System.FilePath ((</>))

import qualified Hadolint.Lint as Lint
import qualified Hadolint.Rules as Rules

type IgnoreRule = Text

type TrustedRegistry = Text

data ConfigFile = ConfigFile
    { ignored :: Maybe [IgnoreRule]
    , trustedRegistries :: Maybe [TrustedRegistry]
    } deriving (Show, Eq, Generic)

instance Yaml.FromJSON ConfigFile

toOutputFormat :: String -> Maybe Lint.OutputFormat
toOutputFormat "json" = Just Lint.Json
toOutputFormat "tty" = Just Lint.TTY
toOutputFormat "codeclimate" = Just Lint.CodeclimateJson
toOutputFormat "checkstyle" = Just Lint.Checkstyle
toOutputFormat "codacy" = Just Lint.Codacy
toOutputFormat _ = Nothing

showFormat :: Lint.OutputFormat -> String
showFormat Lint.Json = "json"
showFormat Lint.TTY = "tty"
showFormat Lint.CodeclimateJson = "codeclimate"
showFormat Lint.Checkstyle = "checkstyle"
showFormat Lint.Codacy = "codacy"

parseOptions :: Parser Lint.LintOptions
parseOptions =
    Lint.LintOptions <$> -- CLI options parser definition
    version <*>
    configFile <*>
    outputFormat <*>
    ignoreList <*>
    files <*>
    parseRulesConfig
  where
    version = switch (long "version" <> short 'v' <> help "Show version")
    --
    -- | Parse the config filename to use
    configFile =
        optional
            (strOption
                 (long "config" <> short 'c' <> metavar "FILENAME" <>
                  help "Path to the configuration file"))
    --
    -- | Parse the output format option
    outputFormat =
        option
            (maybeReader toOutputFormat)
            (long "format" <> -- options for the output format
             short 'f' <>
             help "The output format for the results [tty | json | checkstyle | codeclimate | codacy]" <>
             value Lint.TTY <> -- The default value
             showDefaultWith showFormat <>
             completeWith ["tty", "json", "checkstyle", "codeclimate", "codacy"])
    --
    -- | Parse a list of ignored rules
    ignoreList =
        many
            (strOption
                 (long "ignore" <>
                  help "A rule to ignore. If present, the ignore list in the config file is ignored" <>
                  metavar "RULECODE"))
    --
    -- | Parse a list of dockerfile names
    files = many (argument str (metavar "DOCKERFILE..." <> action "file"))
    --
    -- | Parses all the optional rules configuration
    parseRulesConfig =
        Rules.RulesConfig . Set.fromList . fmap fromString <$>
        many
            (strOption
                 (long "trusted-registry" <>
                  help "A docker registry to allow to appear in FROM instructions" <>
                  metavar "REGISTRY (e.g. docker.io)"))

main :: IO ()
main = execParser opts >>= applyConfig >>= Lint.lint
  where
    opts =
        info
            (helper <*> parseOptions)
            (fullDesc <> progDesc "Lint Dockerfile for errors and best practices" <>
             header "hadolint - Dockerfile Linter written in Haskell")

applyConfig :: Lint.LintOptions -> IO Lint.LintOptions
applyConfig o
    | not (null (Lint.ignoreRules o)) && Lint.rulesConfig o /= mempty = return o
    | otherwise = do
        theConfig <-
            case Lint.configFile o of
                Nothing -> findConfig
                c -> return c
        case theConfig of
            Nothing -> return o
            Just config -> parseAndApply config
  where
    findConfig = do
        localConfigFile <- (</> ".hadolint.yaml") <$> getCurrentDirectory
        configFile <- getXdgDirectory XdgConfig "hadolint.yaml"
        listToMaybe <$> filterM doesFileExist [localConfigFile, configFile]
    parseAndApply config = do
        result <- Yaml.decodeFileEither config
        case result of
            Left err -> printError err config
            Right (ConfigFile ignore trusted) -> return (override ignore trusted)
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
    printError err config =
        case err of
            Yaml.AesonException e ->
                error $
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
                    , e
                    ]
            _ ->
                error $
                "Error parsing your config file in  '" ++
                config ++ "': " ++ Yaml.prettyPrintParseException err

-- | Support UNIX convention of passing "-" instead of "/dev/stdin"
parseFilename :: String -> String
parseFilename "-" = "/dev/stdin"
parseFilename s = s

-- | Helper to analyze AST quickly in GHCI
analyzeEither :: Rules.RulesConfig -> Either t Dockerfile -> [Rules.RuleCheck]
analyzeEither _ (Left _) = []
analyzeEither config (Right dockerFile) = Lint.analyzeAll config dockerFile
