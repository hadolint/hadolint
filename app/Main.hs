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

import qualified Hadolint.Config as Config
import qualified Hadolint.Lint as Lint
import qualified Hadolint.Rules as Rules

data CommandOptions = CommandOptions
    { showVersion :: Bool
    , configFile :: Maybe FilePath
    , format :: Lint.OutputFormat
    , dockerfiles :: [String]
    , lintingOptions :: Lint.LintOptions
    }

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
    -- | Parse the rule ignore list and the rules configuration into a LintOptions
    lintOptions = Lint.LintOptions <$> ignoreList <*> parseRulesConfig
    --
    -- | Parse all the optional rules configuration
    parseRulesConfig =
        Rules.RulesConfig . Set.fromList . fmap fromString <$>
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
        lintConfig <- Config.applyConfig (configFile cmd) (lintingOptions cmd)
        let files = NonEmpty.fromList (dockerfiles cmd)
        case lintConfig of
            Left err -> error err
            Right conf -> do
                res <- Lint.lint conf files
                Lint.printResultsAndExit (format cmd) res
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
