{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Applicative
import Control.Monad (when)
import qualified Data.Bifunctor as Bifunctor
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.String (IsString (fromString))
import qualified Data.Text as Text
import qualified Data.Version
import qualified Development.GitRev
import qualified Hadolint
import qualified Hadolint.Formatter.Format as Format
import qualified Hadolint.Rule as Rule
import Options.Applicative
  ( Parser,
    ReadM,
    action,
    argument,
    completeWith,
    eitherReader,
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
import System.IO (hPutStrLn, stderr)

data CommandOptions = CommandOptions
  { showVersion :: Bool,
    noFail :: Bool,
    nocolor :: Bool,
    configFile :: Maybe FilePath,
    isVerbose :: Bool,
    format :: Hadolint.OutputFormat,
    dockerfiles :: [String],
    lintingOptions :: Hadolint.LintOptions,
    filePathInReportOption :: Maybe FilePath
  }

toOutputFormat :: String -> Maybe Hadolint.OutputFormat
toOutputFormat "json" = Just Hadolint.Json
toOutputFormat "sonarqube" = Just Hadolint.SonarQube
toOutputFormat "tty" = Just Hadolint.TTY
toOutputFormat "codeclimate" = Just Hadolint.CodeclimateJson
toOutputFormat "gitlab_codeclimate" = Just Hadolint.GitlabCodeclimateJson
toOutputFormat "checkstyle" = Just Hadolint.Checkstyle
toOutputFormat "codacy" = Just Hadolint.Codacy
toOutputFormat _ = Nothing

showFormat :: Hadolint.OutputFormat -> String
showFormat Hadolint.Json = "json"
showFormat Hadolint.SonarQube = "sonarqube"
showFormat Hadolint.TTY = "tty"
showFormat Hadolint.CodeclimateJson = "codeclimate"
showFormat Hadolint.GitlabCodeclimateJson = "gitlab_codeclimate"
showFormat Hadolint.Checkstyle = "checkstyle"
showFormat Hadolint.Codacy = "codacy"

toNofailSeverity :: String -> Maybe Rule.DLSeverity
toNofailSeverity "error" = Just Rule.DLErrorC
toNofailSeverity "warning" = Just Rule.DLWarningC
toNofailSeverity "info" = Just Rule.DLInfoC
toNofailSeverity "style" = Just Rule.DLStyleC
toNofailSeverity "ignore" = Just Rule.DLIgnoreC
toNofailSeverity "none" = Just Rule.DLIgnoreC
toNofailSeverity _ = Nothing

parseOptions :: Parser CommandOptions
parseOptions =
  CommandOptions
    <$> version -- CLI options parser definition
    <*> noFail
    <*> nocolor
    <*> configFile
    <*> isVerbose
    <*> outputFormat
    <*> files
    <*> lintOptions
    <*> filePathInReportOption
  where
    version = switch (long "version" <> short 'v' <> help "Show version")

    noFail = switch (long "no-fail" <> help "Don't exit with a failure status code when any rule is violated")

    noFailCutoff =
      option
        (maybeReader toNofailSeverity)
        ( short 't'
            <> long "failure-threshold"
            <> help
              "Exit with failure code only when rules with a severity \
              \equal to or above THRESHOLD are violated. Accepted values: \
              \[error | warning | info | style | ignore | none]"
            <> value Rule.DLInfoC
            <> metavar "THRESHOLD"
            <> showDefaultWith (Text.unpack . Format.severityText)
            <> completeWith ["error", "warning", "info", "style", "ignore", "none"]
        )

    nocolor = switch (long "no-color" <> help "Don't colorize output")

    strictlabels =
      switch
        ( long "strict-labels"
            <> help "Do not permit labels other than specified in `label-schema`"
        )

    configFile =
      optional
        ( strOption
            ( long "config" <> short 'c' <> metavar "FILENAME"
                <> help "Path to the configuration file"
            )
        )

    isVerbose = switch (long "verbose" <> short 'V' <> help "Enables verbose logging of hadolint's output to stderr")

    outputFormat =
      option
        (maybeReader toOutputFormat)
        ( long "format"
            <> short 'f' -- options for the output format
            <> help
              "The output format for the results [tty | json | checkstyle | codeclimate | gitlab_codeclimate | codacy | sonarqube]"
            <> value Hadolint.TTY
            <> showDefaultWith showFormat -- The default value
            <> completeWith ["tty", "json", "checkstyle", "codeclimate", "gitlab_codeclimate", "codacy", "sonarqube"]
        )

    errorList =
      many
        ( strOption
            ( long "error"
                <> help "Make the rule `RULECODE` have the level `error`"
                <> metavar "RULECODE"
            )
        )

    warningList =
      many
        ( strOption
            ( long "warning"
                <> help "Make the rule `RULECODE` have the level `warning`"
                <> metavar "RULECODE"
            )
        )

    infoList =
      many
        ( strOption
            ( long "info"
                <> help "Make the rule `RULECODE` have the level `info`"
                <> metavar "RULECODE"
            )
        )

    styleList =
      many
        ( strOption
            ( long "style"
                <> help "Make the rule `RULECODE` have the level `style`"
                <> metavar "RULECODE"
            )
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

    lintOptions =
      Hadolint.LintOptions
        <$> errorList
        <*> warningList
        <*> infoList
        <*> styleList
        <*> ignoreList
        <*> parseRulesConfig
        <*> noFailCutoff

    filePathInReportOption =
      optional
        ( strOption
            ( long "file-path-in-report" <> metavar "FILEPATHINREPORT"
                <> help "The file path referenced in the generated report. This only applies for the 'checkstyle' format and is useful when running Hadolint with Docker to set the correct file path."
            )
        )

    labels =
      Map.fromList
        <$> many
          ( option
              readSingleLabelSchema
              ( long "require-label"
                  <> help "The option --require-label=label:format makes Hadolint check that the label `label` conforms to format requirement `format`"
                  <> metavar "LABELSCHEMA (e.g. maintainer:text)"
              )
          )

    parseRulesConfig =
      Hadolint.RulesConfig
        <$> parseAllowedRegistries
        <*> labels
        <*> strictlabels

    parseAllowedRegistries =
      Set.fromList . fmap fromString
        <$> many
          ( strOption
              ( long "trusted-registry"
                  <> help "A docker registry to allow to appear in FROM instructions"
                  <> metavar "REGISTRY (e.g. docker.io)"
              )
          )

type SingleLabelSchema = (Rule.LabelName, Rule.LabelType)

readSingleLabelSchema :: ReadM SingleLabelSchema
readSingleLabelSchema = eitherReader $ \s -> labelParser (Text.pack s)

labelParser :: Text.Text -> Either String (Rule.LabelName, Rule.LabelType)
labelParser l =
  case Bifunctor.second (Rule.read . Text.drop 1) $ Text.breakOn ":" l of
    (ln, Right lt) -> Right (ln, lt)
    (_, Left e) -> Left $ Text.unpack e

noFailure :: Hadolint.Result s e -> Rule.DLSeverity -> Bool
noFailure (Hadolint.Result _ Seq.Empty Seq.Empty) _ = True
noFailure (Hadolint.Result _ Seq.Empty fails) cutoff =
  Seq.null (Seq.filter (\f -> Rule.severity f <= cutoff) fails)
noFailure _ _ = False

exitProgram ::
  Foldable f =>
  CommandOptions ->
  Hadolint.LintOptions ->
  f (Hadolint.Result s e) ->
  IO ()
exitProgram cmd conf res
  | noFail cmd = exitSuccess
  | Hadolint.shallSkipErrorStatus (format cmd) = exitSuccess
  | all (`noFailure` Hadolint.failThreshold conf) res = exitSuccess
  | otherwise = exitFailure

runLint :: CommandOptions -> Hadolint.LintOptions -> NonEmpty.NonEmpty String -> IO ()
runLint cmd conf files = do
  res <- Hadolint.lintIO conf files
  noColorEnv <- lookupEnv "NO_COLOR"
  let noColor = nocolor cmd || isJust noColorEnv
  let filePathInReport = filePathInReportOption cmd
  Hadolint.printResults (format cmd) noColor filePathInReport res
  exitProgram cmd conf res

main :: IO ()
main = do
  cmd <- execParser opts
  execute cmd
  where
    execute CommandOptions {showVersion = True} = putStrLn getVersion >> exitSuccess
    execute CommandOptions {dockerfiles = []} =
      putStrLn "Please provide a Dockerfile" >> exitFailure
    execute cmd = do
      maybeConfig <- Hadolint.getConfig (configFile cmd)
      when (isVerbose cmd) (hPutStrLn stderr $ getFilePathDescription maybeConfig)
      lintConfig <- Hadolint.applyConfig maybeConfig (lintingOptions cmd)
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

getFilePathDescription :: Maybe FilePath -> String
getFilePathDescription Nothing = "No configuration was specified. Using default configuration"
getFilePathDescription (Just filepath) = "Configuration file used: " ++ filepath
