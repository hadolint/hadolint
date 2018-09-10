{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Control.Applicative
import qualified Data.List.NonEmpty as NonEmpty
import Data.Semigroup ((<>))
import qualified Data.Set as Set
import Data.String
import qualified Data.Version
import qualified Development.GitRev
import Options.Applicative hiding (ParseError)
import qualified Paths_hadolint -- version from hadolint.cabal file
import System.Exit (exitFailure, exitSuccess)

import qualified Hadolint

data CommandOptions = CommandOptions
    { showVersion :: Bool
    , configFile :: Maybe FilePath
    , format :: Hadolint.OutputFormat
    , dockerfiles :: [String]
    , lintingOptions :: Hadolint.LintOptions
    }

toOutputFormat :: String -> Maybe Hadolint.OutputFormat
toOutputFormat "json" = Just Hadolint.Json
toOutputFormat "tty" = Just Hadolint.TTY
toOutputFormat "codeclimate" = Just Hadolint.CodeclimateJson
toOutputFormat "checkstyle" = Just Hadolint.Checkstyle
toOutputFormat "codacy" = Just Hadolint.Codacy
toOutputFormat _ = Nothing

showFormat :: Hadolint.OutputFormat -> String
showFormat Hadolint.Json = "json"
showFormat Hadolint.TTY = "tty"
showFormat Hadolint.CodeclimateJson = "codeclimate"
showFormat Hadolint.Checkstyle = "checkstyle"
showFormat Hadolint.Codacy = "codacy"

parseOptions :: Parser CommandOptions
parseOptions =
    CommandOptions <$> -- CLI options parser definition
    version <*>
    configFile <*>
    outputFormat <*>
    files <*>
    lintOptions
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
             help
                 "The output format for the results [tty | json | checkstyle | codeclimate | codacy]" <>
             value Hadolint.TTY <> -- The default value
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
    -- | Parse the rule ignore list and the rules configuration into a LintOptions
    lintOptions = Hadolint.LintOptions <$> ignoreList <*> parseRulesConfig
    --
    -- | Parse all the optional rules configuration
    parseRulesConfig =
        Hadolint.RulesConfig . Set.fromList . fmap fromString <$>
        many
            (strOption
                 (long "trusted-registry" <>
                  help "A docker registry to allow to appear in FROM instructions" <>
                  metavar "REGISTRY (e.g. docker.io)"))

main :: IO ()
main = do
    cmd <- execParser opts
    execute cmd
  where
    execute CommandOptions {showVersion = True} = putStrLn getVersion >> exitSuccess
    execute CommandOptions {dockerfiles = []} =
        putStrLn "Please provide a Dockerfile" >> exitFailure
    execute cmd = do
        lintConfig <- Hadolint.applyConfig (configFile cmd) (lintingOptions cmd)
        let files = NonEmpty.fromList (dockerfiles cmd)
        case lintConfig of
            Left err -> error err
            Right conf -> do
                res <- Hadolint.lint conf files
                Hadolint.printResultsAndExit (format cmd) res
    opts =
        info
            (helper <*> parseOptions)
            (fullDesc <> progDesc "Lint Dockerfile for errors and best practices" <>
             header "hadolint - Dockerfile Linter written in Haskell")

getVersion :: String
getVersion
    | $(Development.GitRev.gitDescribe) == "UNKNOWN" =
        "Haskell Dockerfile Linter " ++ Data.Version.showVersion Paths_hadolint.version ++ "-no-git"
    | otherwise = "Haskell Dockerfile Linter " ++ $(Development.GitRev.gitDescribe)
