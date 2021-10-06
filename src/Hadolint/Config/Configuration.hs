module Hadolint.Config.Configuration
  ( Configuration (..)
  )
where

import Data.Coerce (coerce)
import Data.Default
import Data.Text (Text)
import Data.YAML ((.:?), (.!=))
import GHC.Generics (Generic)
import Hadolint.Formatter.Format (OutputFormat (..))
import Hadolint.Lint (LintOptions (..))
import Hadolint.Meta ((<>>))
import Hadolint.Process (RulesConfig (..))
import Hadolint.Rule (RuleCode (..), DLSeverity (..))
import Language.Docker
import qualified Data.Set as Set
import qualified Data.YAML as Yaml


data Configuration =
  Configuration
    { noFail :: Maybe Bool,
      noColor :: Maybe Bool,
      verbose :: Maybe Bool,
      format :: Maybe OutputFormat,
      lintingOptions :: LintOptions,
      failThreshold :: Maybe DLSeverity
    }
  deriving (Eq)


instance Show Configuration where
  show c =
    Prelude.unlines
      [ "Configuration:",
        "  no fail: " ++ maybe "unset" show (noFail c),
        "  no color: " ++ maybe "unset" show (noColor c),
        "  output format: " ++ maybe "unset" show (format c),
        "  failure threshold: " ++ maybe "unset" show (failThreshold c)
      ]
      ++ Prelude.unlines
          (fmap ("  " ++) (Prelude.lines $ show (lintingOptions c)))

instance Semigroup Configuration where
  Configuration a1 a2 a3 a4 a5 a6 <> Configuration b1 b2 b3 b4 b5 b6 =
    Configuration
      (a1 <>> b1)
      (a2 <>> b2)
      (a3 <>> b3)
      (a4 <> b4)
      (a5 <> b5)
      (a6 <> b6)

instance Monoid Configuration where
  mempty = Configuration Nothing Nothing Nothing mempty mempty mempty

instance Default Configuration where
  def =
    Configuration
      (Just False)
      (Just False)
      (Just False)
      (Just TTY)
      def
      (Just DLInfoC)

instance Yaml.FromYAML Configuration where
  parseYAML = Yaml.withMap "Configuration" $ \m -> do
    noFail <- m .:? "no-fail" .!= Nothing
    noColor <- m .:? "no-color" .!= Nothing
    verbose <- m .:? "verbose" .!= Nothing
    format <- m .:? "output-format"
    override <- m .:? "override" .!= mempty
    ignored <- m .:? "ignored" .!= mempty
    trusted <- m .:? "trusted-registries" .!= mempty
    labelSchema <- m .:? "label-schema" .!= mempty
    strictLabels <- m .:? "strict-labels" .!= Nothing
    let ignoreRules = coerce (ignored :: [Text])
        errorRules = overrideErrorRules override
        warningRules = overrideWarningRules override
        infoRules = overrideInfoRules override
        styleRules = overrideStyleRules override
        allowedRegistries = Set.fromList (coerce (trusted :: [Text]))
    let rulesConfig = RulesConfig {..}
    let lintingOptions = LintOptions {..}
    failThreshold <- m .:? "failure-threshold"
    return Configuration {..}


data OverrideConfig = OverrideConfig
  { overrideErrorRules :: [RuleCode],
    overrideWarningRules :: [RuleCode],
    overrideInfoRules :: [RuleCode],
    overrideStyleRules :: [RuleCode]
  }
  deriving (Show, Eq, Generic)

instance Semigroup OverrideConfig where
  OverrideConfig a1 a2 a3 a4 <> OverrideConfig b1 b2 b3 b4 =
    OverrideConfig (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4)

instance Monoid OverrideConfig where
  mempty = OverrideConfig mempty mempty mempty mempty

instance Yaml.FromYAML OverrideConfig where
  parseYAML = Yaml.withMap "OverrideConfig" $ \m -> do
    err <- m .:? "error" .!= mempty
    wrn <- m .:? "warning" .!= mempty
    inf <- m .:? "info" .!= mempty
    sty <- m .:? "style" .!= mempty
    let overrideErrorRules = coerce (err :: [Text])
        overrideWarningRules = coerce (wrn :: [Text])
        overrideInfoRules = coerce (inf :: [Text])
        overrideStyleRules = coerce (sty:: [Text])
    return OverrideConfig {..}
