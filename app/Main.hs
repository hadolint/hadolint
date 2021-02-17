{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Applicative
import Data.Semigroup ((<>))
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import qualified Data.List.NonEmpty as NonEmpty
import Data.String (IsString (fromString))
import qualified Data.Version
import qualified Development.GitRev
import qualified Hadolint
import Data.Maybe
import Options.Applicative
  ( Parser,
    action,
    argument,
    completeWith,
    execParser,
    fullDesc,
    header,
    help,
    helper,
    info,
    long,
    maybeReader,
    metavar,
    option,
    progDesc,
    short,
    showDefaultWith,
    str,
    strOption,
    switch,
    value,
  )
-- version from hadolint.cabal file
import qualified Paths_hadolint as Meta
import System.Environment
import System.Exit (exitFailure, exitSuccess)

data CommandOptions = CommandOptions
  { showVersion :: Bool,
    noFail :: Bool,
    nocolor :: Bool,
    configFile :: Maybe FilePath,
    format :: Hadolint.OutputFormat,
    dockerfiles :: [String],
    lintingOptions :: Hadolint.LintOptions
  }

toOutputFormat :: String -> Maybe Hadolint.OutputFormat
toOutputFormat "json" = Just Hadolint.Json
toOutputFormat "tty" = Just Hadolint.TTY
toOutputFormat "codeclimate" = Just Hadolint.CodeclimateJson
toOutputFormat "gitlab_codeclimate" = Just Hadolint.GitlabCodeclimateJson
toOutputFormat "checkstyle" = Just Hadolint.Checkstyle
toOutputFormat "codacy" = Just Hadolint.Codacy
toOutputFormat _ = Nothing

showFormat :: Hadolint.OutputFormat -> String
showFormat Hadolint.Json = "json"
showFormat Hadolint.TTY = "tty"
showFormat Hadolint.CodeclimateJson = "codeclimate"
showFormat Hadolint.GitlabCodeclimateJson = "gitlab_codeclimate"
showFormat Hadolint.Checkstyle = "checkstyle"
showFormat Hadolint.Codacy = "codacy"

parseOptions :: Parser CommandOptions
parseOptions =
  CommandOptions
    <$> version -- CLI options parser definition
    <*> noFail
    <*> nocolor
    <*> configFile
    <*> outputFormat
    <*> files
    <*> lintOptions
  where
    version = switch (long "version" <> short 'v' <> help "Show version")

    noFail = switch (long "no-fail" <> help "Don't exit with a failure status code when any rule is violated")

    nocolor = switch (long "no-color" <> help "Don't colorize output")

    configFile =
      optional
        ( strOption
            ( long "config" <> short 'c' <> metavar "FILENAME"
                <> help "Path to the configuration file"
            )
        )

    outputFormat =
      option
        (maybeReader toOutputFormat)
        ( long "format"
            <> short 'f' -- options for the output format
            <> help
              "The output format for the results [tty | json | checkstyle | codeclimate | gitlab_codeclimate | codacy]"
            <> value Hadolint.TTY
            <> showDefaultWith showFormat -- The default value
            <> completeWith ["tty", "json", "checkstyle", "codeclimate", "gitlab_codeclimate", "codacy"]
        )

    ignoreList =
      many
        ( strOption
            ( long "ignore"
                <> help "A rule to ignore. If present, the ignore list in the config file is ignored"
                <> metavar "RULECODE"
            )
        )

    files = many (argument str (metavar "DOCKERFILE..." <> action "file"))

    lintOptions = Hadolint.LintOptions <$> ignoreList <*> parseRulesConfig

    parseRulesConfig =
      Hadolint.RulesConfig . Set.fromList . fmap fromString
        <$> many
          ( strOption
              ( long "trusted-registry"
                  <> help "A docker registry to allow to appear in FROM instructions"
                  <> metavar "REGISTRY (e.g. docker.io)"
              )
          )

noFailure :: Hadolint.Result s e -> Bool
noFailure (Hadolint.Result Seq.Empty Seq.Empty) = True
noFailure _ = False

exitProgram :: CommandOptions -> Hadolint.Result s e -> IO()
exitProgram cmd res
  | noFail cmd                                 = exitSuccess
  | Hadolint.shallSkipErrorStatus (format cmd) = exitSuccess
  | noFailure res                              = exitSuccess
  | otherwise                                  = exitFailure

runLint :: CommandOptions -> Hadolint.LintOptions -> NonEmpty.NonEmpty String -> IO()
runLint cmd conf files = do
  res <-  Hadolint.lint conf files
  noColorEnv <- lookupEnv "NO_COLOR"
  let noColor = nocolor cmd || isJust noColorEnv
  Hadolint.printResults (format cmd) noColor res
  exitProgram cmd res

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
        Right conf -> runLint cmd conf files
    opts =
      info
        (helper <*> parseOptions)
        ( fullDesc <> progDesc "Lint Dockerfile for errors and best practices"
            <> header "hadolint - Dockerfile Linter written in Haskell"
        )

getVersion :: String
getVersion
  | version == "UNKNOWN" =
    "Haskell Dockerfile Linter " ++ Data.Version.showVersion Meta.version ++ "-no-git"
  | otherwise = "Haskell Dockerfile Linter " ++ version
  where
    version = $(Development.GitRev.gitDescribe)
