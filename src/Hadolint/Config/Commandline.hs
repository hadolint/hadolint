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
import Hadolint.Formatter.Format (readMaybeOutputFormat)
import Hadolint.Config.Configuration
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
      configuration :: PartialConfiguration
    }
  deriving (Eq, Show)

parseCommandline :: Parser CommandlineConfig
parseCommandline =
  CommandlineConfig
    <$> parseVersion
    <*> parseConfigFile
    <*> parseFiles
    <*> parseFilePathInReportOption
    <*> parseConfiguration
  where
    parseVersion = switch (long "version" <> short 'v' <> help "Show version")

    parseConfigFile =
      optional
        ( strOption
            ( long "config" <> short 'c' <> metavar "FILENAME"
                <> help "Path to the configuration file"
            )
        )

    parseFiles = many (argument str (metavar "DOCKERFILE..." <> action "file"))

    parseFilePathInReportOption =
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

    parseConfiguration =
      PartialConfiguration
        <$> parseNoFail
        <*> parseNoColor
        <*> parseVerbose
        <*> parseOutputFormat
        <*> parseErrorList
        <*> parseWarningList
        <*> parseInfoList
        <*> parseStyleList
        <*> parseIgnoreList
        <*> parseAllowedRegistries
        <*> parseLabelSchema
        <*> parseStrictlabels
        <*> parseDisableIgnorePragma
        <*> parseFailureThreshold

    -- All optional flags with boolean value must not have a default value. The
    -- optional parser then converts it to Nothing.
    -- This is to ensure if they are not set, this is correctly mirrored in the
    -- parsed Configuration and then their behaviour with respect to
    -- configurations from environment variables or config files is correct.
    parseNoFail =
      optional
        ( flag' True
            ( long "no-fail"
                <> help "Don't exit with a failure status code when any rule is\
                        \ violated"
            )
        )

    parseNoColor =
      optional
        ( flag' True
            ( long "no-color"
              <> help "Don't colorize output"
            )
        )

    parseVerbose =
      optional
        ( flag' True
            ( long "verbose"
                <> short 'V'
                <> help "Enables verbose logging of hadolint's output to stderr"
            )
        )

    parseOutputFormat =
      optional $
        option
          ( maybeReader (readMaybeOutputFormat . pack) )
          ( long "format"
              <> short 'f' -- options for the output format
              <> help
                "The output format for the results [tty | json | checkstyle |\
                \ codeclimate | gitlab_codeclimate | gnu | codacy | sonarqube |\
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

    parseErrorList =
      many
        ( strOption
            ( long "error"
                <> help "Make the rule `RULECODE` have the level `error`"
                <> metavar "RULECODE"
            )
        )

    parseWarningList =
      many
        ( strOption
            ( long "warning"
                <> help "Make the rule `RULECODE` have the level `warning`"
                <> metavar "RULECODE"
            )
        )

    parseInfoList =
      many
        ( strOption
            ( long "info"
                <> help "Make the rule `RULECODE` have the level `info`"
                <> metavar "RULECODE"
            )
        )

    parseStyleList =
      many
        ( strOption
            ( long "style"
                <> help "Make the rule `RULECODE` have the level `style`"
                <> metavar "RULECODE"
            )
        )

    parseIgnoreList =
      many
        ( strOption
            ( long "ignore"
                <> metavar "RULECODE"
                <> help "A rule to ignore. If present, the ignore list in the\
                        \ config file is ignored"
            )
        )

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

    parseLabelSchema =
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

    parseStrictlabels =
      optional
        ( flag' True
            ( long "strict-labels"
                <> help "Do not permit labels other than specified in\
                        \ `label-schema`"
            )
        )

    parseDisableIgnorePragma =
      optional
        ( flag' True
            ( long "disable-ignore-pragma"
                <> help "Disable inline ignore pragmas \
                        \ `# hadolint ignore=DLxxxx`"
            )
        )

    parseFailureThreshold =
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
