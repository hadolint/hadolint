module Hadolint.Config.Commandline
  ( CommandlineConfig (..),
    parseCommandline
  )
where

import Control.Applicative
import Data.Bifunctor (second)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.String (IsString (fromString))
import Data.Text (Text, pack, unpack, breakOn, drop)
import Hadolint.Lint (LintOptions (..))
import Hadolint.Process (RulesConfig (..))
import Hadolint.Formatter.Format (readMaybeOutputFormat)
import Hadolint.Config.Configuration (Configuration (..))
import Hadolint.Rule
  ( LabelName,
    LabelType,
    readEitherLabelType,
    readEitherSeverity
  )
import Options.Applicative
  ( Parser,
    ReadM,
    action,
    argument,
    completeWith,
    eitherReader,
    flag',
    help,
    long,
    maybeReader,
    metavar,
    option,
    short,
    str,
    strOption,
    switch,
  )


data CommandlineConfig =
  CommandlineConfig
    { showVersion :: Bool,
      configFile :: Maybe FilePath,
      dockerfiles :: [String],
      filePathInReportOption :: Maybe FilePath,
      configuration :: Configuration
    }
  deriving (Eq, Show)

parseCommandline :: Parser CommandlineConfig
parseCommandline =
  CommandlineConfig
    <$> version
    <*> configFile
    <*> files
    <*> filePathInReportOption
    <*> configuration
  where
    version = switch (long "version" <> short 'v' <> help "Show version")

    configFile =
      optional
        ( strOption
            ( long "config" <> short 'c' <> metavar "FILENAME"
                <> help "Path to the configuration file"
            )
        )

    files = many (argument str (metavar "DOCKERFILE..." <> action "file"))

    filePathInReportOption =
      optional
        ( strOption
            ( long "file-path-in-report"
                <> metavar "FILEPATHINREPORT"
                <> help "The file path referenced in the generated report.\
                        \ This only applies for the 'checkstyle' format and is\
                        \ useful when running Hadolint with Docker to set the\
                        \ correct file path."
            )
        )

    configuration =
      Configuration
        <$> noFail
        <*> nocolor
        <*> isVerbose
        <*> outputFormat
        <*> lintOptions
        <*> noFailCutoff

    -- All optional flags with boolean value must not have a default value. The
    -- optional parser then converts it to Nothing.
    -- This is to ensure if they are not set, this is correctly mirrored in the
    -- parsed Configuration and then their behaviour with respect to
    -- configurations from environment variables or config files is correct.
    noFail =
      optional
        ( flag' True
            ( long "no-fail"
                <> help "Don't exit with a failure status code when any rule is\
                        \ violated"
            )
        )

    nocolor =
      optional
        ( flag' True
            ( long "no-color"
              <> help "Don't colorize output"
            )
        )

    isVerbose =
      optional
        ( flag' True
            ( long "verbose"
                <> short 'V'
                <> help "Enables verbose logging of hadolint's output to stderr"
            )
        )

    outputFormat =
      optional $
        option
          ( maybeReader (readMaybeOutputFormat . pack) )
          ( long "format"
              <> short 'f' -- options for the output format
              <> help
                "The output format for the results [tty | json | checkstyle |\
                \ codeclimate | gitlab_codeclimate | codacy | sonarqube |\
                \ sarif] (default: tty)"
              <> completeWith
                  [ "tty",
                    "json",
                    "checkstyle",
                    "codeclimate",
                    "gitlab_codeclimate",
                    "codacy",
                    "sonarqube",
                    "sarif"
                  ]
          )

    lintOptions =
      LintOptions
        <$> errorList
        <*> warningList
        <*> infoList
        <*> styleList
        <*> ignoreList
        <*> parseRulesConfig

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
                <> metavar "RULECODE"
                <> help "A rule to ignore. If present, the ignore list in the\
                        \ config file is ignored"
            )
        )

    parseRulesConfig =
      RulesConfig
        <$> parseAllowedRegistries
        <*> labels
        <*> strictlabels

    parseAllowedRegistries =
      Set.fromList . fmap fromString
        <$> many
          ( strOption
              ( long "trusted-registry"
                  <> metavar "REGISTRY (e.g. docker.io)"
                  <> help "A docker registry to allow to appear in FROM \
                          \instructions"
              )
          )

    labels =
      Map.fromList
        <$> many
          ( option
              readSingleLabelSchema
              ( long "require-label"
                  <> metavar "LABELSCHEMA (e.g. maintainer:text)"
                  <> help "The option --require-label=label:format makes\
                          \ Hadolint check that the label `label` conforms to\
                          \ format requirement `format`"
              )
          )

    strictlabels =
      optional
        ( flag' True
            ( long "strict-labels"
                <> help "Do not permit labels other than specified in\
                        \ `label-schema`"
            )
        )

    noFailCutoff =
      optional $
        option
          ( eitherReader (readEitherSeverity . pack) )
          ( short 't'
              <> long "failure-threshold"
              <> help
                "Exit with failure code only when rules with a severity equal\
                \ to or above THRESHOLD are violated. Accepted values: [error\
                \ | warning | info | style | ignore | none] (default: info)"
              <> metavar "THRESHOLD"
              <> completeWith
                  [ "error",
                    "warning",
                    "info",
                    "style",
                    "ignore",
                    "none"
                  ]
          )


type SingleLabelSchema = (LabelName, LabelType)

readSingleLabelSchema :: ReadM SingleLabelSchema
readSingleLabelSchema = eitherReader $ \s -> labelParser (pack s)

labelParser :: Text -> Either String (LabelName, LabelType)
labelParser l =
  case second (readEitherLabelType . Data.Text.drop 1) $ breakOn ":" l of
    (ln, Right lt) -> Right (ln, lt)
    (_, Left e) -> Left (unpack e)
