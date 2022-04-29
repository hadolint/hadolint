module Hadolint.Config.Configuration
  ( Configuration (..),
    PartialConfiguration (..),

    applyPartialConfiguration,
  )
where

import Data.Maybe (fromMaybe)
import Control.Applicative
import Data.Coerce (coerce)
import Data.Default
import Data.Text (Text)
import Data.YAML ((.:?), (.!=))
import GHC.Generics (Generic)
import Hadolint.Formatter.Format (OutputFormat (..))
import Hadolint.Rule (RuleCode (..), DLSeverity (..), LabelSchema)
import Language.Docker
import Prettyprinter
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.YAML as Yaml


data Configuration =
  Configuration
    { noFail :: Bool,
      noColor :: Bool,
      verbose :: Bool,
      format :: OutputFormat,
      errorRules :: [RuleCode],
      warningRules :: [RuleCode],
      infoRules :: [RuleCode],
      styleRules :: [RuleCode],
      ignoreRules :: [RuleCode],
      allowedRegistries :: Set.Set Registry,
      labelSchema :: LabelSchema,
      strictLabels :: Bool,
      disableIgnorePragma :: Bool,
      failureThreshold :: DLSeverity
    }
  deriving (Eq, Show)

instance Default Configuration where
  def =
    Configuration
      False
      False
      False
      def
      mempty
      mempty
      mempty
      mempty
      mempty
      mempty
      mempty
      False
      False
      def

applyPartialConfiguration ::
  Configuration -> PartialConfiguration -> Configuration
applyPartialConfiguration config partial =
  Configuration
    (fromMaybe (noFail config) (partialNoFail partial))
    (fromMaybe (noColor config) (partialNoColor partial))
    (fromMaybe (verbose config) (partialVerbose partial))
    (fromMaybe (format config) (partialFormat partial))
    (errorRules config <> partialErrorRules partial)
    (warningRules config <> partialWarningRules partial)
    (infoRules config <> partialInfoRules partial)
    (styleRules config <> partialStyleRules partial)
    (ignoreRules config <> partialIgnoreRules partial)
    (allowedRegistries config <> partialAllowedRegistries partial)
    (labelSchema config <> partialLabelSchema partial)
    (fromMaybe (strictLabels config) (partialStrictLabels partial))
    (fromMaybe (disableIgnorePragma config) (partialDisableIgnorePragma partial))
    (fromMaybe (failureThreshold config) (partialFailureThreshold partial))

instance Pretty Configuration where
  pretty c =
    nest 2
      ( vsep
          [ "Configuration:",
            "no fail:" <+> pretty (noFail c),
            "no color:" <+> pretty (noColor c),
            "output format:" <+> pretty (format c),
            "failure threshold:" <+> pretty (failureThreshold c),
            prettyPrintRulelist "error" (errorRules c),
            prettyPrintRulelist "warning" (warningRules c),
            prettyPrintRulelist "info" (infoRules c),
            prettyPrintRulelist "style" (styleRules c),
            prettyPrintRulelist "ignore" (ignoreRules c),
            "strict labels:" <+> pretty (strictLabels c),
            prettyPrintLabelSchema (labelSchema c),
            "disable ignore pragma:" <+> pretty (disableIgnorePragma c),
            prettyPrintRegistries (allowedRegistries c)
          ]
      )

prettyPrintRulelist :: String -> [RuleCode] -> Doc ann
prettyPrintRulelist name lst =
  nest 2 (("override" <+> pretty name <> ":\n") <> prettyPrintList pretty lst)

-- | This function needs to convert the set to a list because Doc ann is not
-- ordered.
prettyPrintRegistries :: Set.Set Registry -> Doc ann
prettyPrintRegistries regs =
  nest 2 ( "allowed registries:\n"
             <> prettyPrintList
                 (\r -> "-" <+> pretty (unRegistry r))
                 (Set.toList regs)
         )

prettyPrintLabelSchema :: LabelSchema -> Doc ann
prettyPrintLabelSchema ls =
  nest 2 ( "label schema:\n"
             <> prettyPrintList
                  (\(n, t) -> pretty n <> ":" <+> pretty t)
                  (Map.toList ls)
         )

-- | pretty print a list with a custom pretty printing function for each element
prettyPrintList :: (a -> Doc ann) -> [a] -> Doc ann
prettyPrintList _ [] = "none"
prettyPrintList prnt lst = vsep (fmap (\i -> "-" <+> prnt i) lst)


data PartialConfiguration =
  PartialConfiguration
    { partialNoFail :: Maybe Bool,
      partialNoColor :: Maybe Bool,
      partialVerbose :: Maybe Bool,
      partialFormat :: Maybe OutputFormat,
      partialErrorRules :: [RuleCode],
      partialWarningRules :: [RuleCode],
      partialInfoRules :: [RuleCode],
      partialStyleRules :: [RuleCode],
      partialIgnoreRules :: [RuleCode],
      partialAllowedRegistries :: Set.Set Registry,
      partialLabelSchema :: LabelSchema,
      partialStrictLabels :: Maybe Bool,
      partialDisableIgnorePragma :: Maybe Bool,
      partialFailureThreshold :: Maybe DLSeverity
    }
  deriving (Eq, Show)


instance Semigroup PartialConfiguration where
  PartialConfiguration a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14
    <> PartialConfiguration b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 =
      PartialConfiguration
        (b1 <|> a1)
        (b2 <|> a2)
        (b3 <|> a3)
        (a4 <> b4)
        (a5 <> b5)
        (a6 <> b6)
        (a7 <> b7)
        (a8 <> b8)
        (a9 <> b9)
        (a10 <> b10)
        (a11 <> b11)
        (b12 <|> a12)
        (b13 <|> a13)
        (a14 <> b14)

instance Monoid PartialConfiguration where
  mempty =
    PartialConfiguration
      Nothing
      Nothing
      Nothing
      mempty
      mempty
      mempty
      mempty
      mempty
      mempty
      mempty
      mempty
      Nothing
      Nothing
      mempty

instance Default PartialConfiguration where
  def = mempty

instance Yaml.FromYAML PartialConfiguration where
  parseYAML = Yaml.withMap "Configuration" $ \m -> do
    partialNoFail <- m .:? "no-fail" .!= Nothing
    partialNoColor <- m .:? "no-color" .!= Nothing
    partialVerbose <- m .:? "verbose" .!= Nothing
    partialFormat <- m .:? "format"
    override <- m .:? "override" .!= mempty
    ignored <- m .:? "ignored" .!= mempty
    trusted <- m .:? "trustedRegistries" .!= mempty
    partialLabelSchema <- m .:? "label-schema" .!= mempty
    partialStrictLabels <- m .:? "strict-labels" .!= Nothing
    partialDisableIgnorePragma <- m .:? "disable-ignore-pragma" .!= Nothing
    let partialIgnoreRules = coerce (ignored :: [Text])
        partialErrorRules = overrideErrorRules override
        partialWarningRules = overrideWarningRules override
        partialInfoRules = overrideInfoRules override
        partialStyleRules = overrideStyleRules override
        partialAllowedRegistries = Set.fromList (coerce (trusted :: [Text]))
    partialFailureThreshold <- m .:? "failure-threshold"
    return PartialConfiguration {..}


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
