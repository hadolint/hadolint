module Hadolint.Config.Configuration
  ( Configuration (..)
  )
where

import Control.Applicative ((<|>))
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
    { noFail :: Maybe Bool,
      noColor :: Maybe Bool,
      verbose :: Maybe Bool,
      format :: Maybe OutputFormat,
      errorRules :: [RuleCode],
      warningRules :: [RuleCode],
      infoRules :: [RuleCode],
      styleRules :: [RuleCode],
      ignoreRules :: [RuleCode],
      allowedRegistries :: Set.Set Registry,
      labelSchema :: LabelSchema,
      strictLabels :: Maybe Bool,
      failThreshold :: Maybe DLSeverity
    }
  deriving (Eq, Show)


instance Pretty Configuration where
  pretty c =
    nest 2
      ( vsep
          [ "Configuration:",
            "no fail:" <+> maybe "unset" pretty (noFail c),
            "no color:" <+> maybe "unset" pretty (noColor c),
            "output format:" <+> maybe "unset" pretty (format c),
            "failure threshold:" <+> maybe "unset" pretty (failThreshold c),
            prettyPrintRulelist "error" (errorRules c),
            prettyPrintRulelist "warning" (warningRules c),
            prettyPrintRulelist "info" (infoRules c),
            prettyPrintRulelist "style" (styleRules c),
            prettyPrintRulelist "ignore" (ignoreRules c),
            "strict labels:" <+> maybe "unset" pretty (strictLabels c),
            prettyPrintLabelSchema (labelSchema c),
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

instance Semigroup Configuration where
  Configuration a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13
    <> Configuration b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 =
      Configuration
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
        (a13 <> b13)

instance Monoid Configuration where
  mempty =
    Configuration
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
      mempty

instance Default Configuration where
  def =
    Configuration
      (Just False)
      (Just False)
      (Just False)
      (Just def)
      mempty
      mempty
      mempty
      mempty
      mempty
      mempty
      mempty
      (Just False)
      (Just def)

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
