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
import qualified Data.Version
import qualified Data.Yaml as Yaml
import qualified Development.GitRev
import GHC.Generics
import qualified Language.Docker as Docker
import Language.Docker.Syntax (Dockerfile)
import Options.Applicative hiding (ParseError)
import qualified Paths_hadolint -- version from hadolint.cabal file
import System.Directory
       (XdgDirectory(..), doesFileExist, getCurrentDirectory,
        getXdgDirectory)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath ((</>))

import qualified Hadolint.Formatter.Checkstyle as Checkstyle
import qualified Hadolint.Formatter.Codeclimate as Codeclimate
import qualified Hadolint.Formatter.Format as Format
import qualified Hadolint.Formatter.Json as Json
import qualified Hadolint.Formatter.TTY as TTY
import qualified Hadolint.Formatter.Codacy as Codacy
import qualified Hadolint.Rules as Rules

type IgnoreRule = Text

type TrustedRegistry = Text

data OutputFormat
    = Json
    | TTY
    | CodeclimateJson
    | Checkstyle
    | Codacy
    deriving (Show, Eq)

data LintOptions = LintOptions
    { showVersion :: Bool
    , configFile :: Maybe FilePath
    , format :: OutputFormat
    , ignoreRules :: [IgnoreRule]
    , dockerfiles :: [String]
    , rulesConfig :: Rules.RulesConfig
    } deriving (Show)

data ConfigFile = ConfigFile
    { ignored :: Maybe [IgnoreRule]
    , trustedRegistries :: Maybe [TrustedRegistry]
    } deriving (Show, Eq, Generic)

instance Yaml.FromJSON ConfigFile

ignoreFilter :: [IgnoreRule] -> Rules.RuleCheck -> Bool
ignoreFilter ignoredRules (Rules.RuleCheck (Rules.Metadata code _ _) _ _ _) =
    code `notElem` ignoredRules

toOutputFormat :: String -> Maybe OutputFormat
toOutputFormat "json" = Just Json
toOutputFormat "tty" = Just TTY
toOutputFormat "codeclimate" = Just CodeclimateJson
toOutputFormat "checkstyle" = Just Checkstyle
toOutputFormat "codacy" = Just Codacy
toOutputFormat _ = Nothing

showFormat :: OutputFormat -> String
showFormat Json = "json"
showFormat TTY = "tty"
showFormat CodeclimateJson = "codeclimate"
showFormat Checkstyle = "checkstyle"
showFormat Codacy = "codacy"

parseOptions :: Parser LintOptions
parseOptions =
    LintOptions <$> -- CLI options parser definition
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
             value TTY <> -- The default value
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
main = execParser opts >>= applyConfig >>= lint
  where
    opts =
        info
            (helper <*> parseOptions)
            (fullDesc <> progDesc "Lint Dockerfile for errors and best practices" <>
             header "hadolint - Dockerfile Linter written in Haskell")

applyConfig :: LintOptions -> IO LintOptions
applyConfig o
    | not (null (ignoreRules o)) && rulesConfig o /= mempty = return o
    | otherwise = do
        theConfig <-
            case configFile o of
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
    -- | Applies the configuration found in the file to the passed LintOptions
    override ignore trusted = applyTrusted trusted . applyIgnore ignore $ o
    applyIgnore ignore opts =
        case ignoreRules opts of
            [] -> opts {ignoreRules = fromMaybe [] ignore}
            _ -> opts
    applyTrusted trusted opts
        | null (Rules.allowedRegistries (rulesConfig opts)) =
            opts {rulesConfig = toRules trusted <> rulesConfig opts}
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

getVersion :: String
getVersion
    | $(Development.GitRev.gitDescribe) == "UNKNOWN" =
        "Haskell Dockerfile Linter " ++ Data.Version.showVersion Paths_hadolint.version ++ "-no-git"
    | otherwise = "Haskell Dockerfile Linter " ++ $(Development.GitRev.gitDescribe)

-- | Performs the process of parsing the dockerfile and analyzing it with all the applicable
-- rules, depending on the list of ignored rules.
-- Depending on the preferred printing format, it will output the results to stdout
lint :: LintOptions -> IO ()
lint LintOptions {showVersion = True} = putStrLn getVersion >> exitSuccess
lint LintOptions {dockerfiles = []} = putStrLn "Please provide a Dockerfile" >> exitFailure
lint LintOptions {ignoreRules = ignoreList, dockerfiles = dFiles, format, rulesConfig} = do
    processedFiles <- mapM (lintDockerfile ignoreList) dFiles
    let allResults = results processedFiles
    printResult allResults
    if allResults /= mempty
        then exitFailure
        else exitSuccess
  where
    results = foldMap Format.toResult -- Parse and check rules for each dockerfile,
                                      -- then convert them to a Result and combine with
                                      -- the result of the previous dockerfile results
    printResult res =
        case format of
            TTY -> TTY.printResult res
            Json -> Json.printResult res
            Checkstyle -> Checkstyle.printResult res
            CodeclimateJson -> Codeclimate.printResult res >> exitSuccess
            Codacy -> Codacy.printResult res >> exitSuccess
    lintDockerfile ignoreRules dockerFile = do
        ast <- Docker.parseFile (parseFilename dockerFile)
        return (processedFile ast)
      where
        processedFile = fmap processRules
        processRules fileLines = filter ignoredRules (analyzeAll rulesConfig fileLines)
        ignoredRules = ignoreFilter ignoreRules

-- | Returns the result of applying all the rules to the given dockerfile
analyzeAll :: Rules.RulesConfig -> Dockerfile -> [Rules.RuleCheck]
analyzeAll config = Rules.analyze (Rules.rules ++ Rules.optionalRules config)

-- | Helper to analyze AST quickly in GHCI
analyzeEither :: Rules.RulesConfig -> Either t Dockerfile -> [Rules.RuleCheck]
analyzeEither _ (Left _) = []
analyzeEither config (Right dockerFile) = analyzeAll config dockerFile
