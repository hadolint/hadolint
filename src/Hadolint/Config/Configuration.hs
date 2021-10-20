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
    "Configuration:\n"
      <> "  no fail: " <> maybe "unset" pretty (noFail c) <> "\n"
      <> "  no color: " <> maybe "unset" pretty (noColor c) <> "\n"
      <> "  output format: " <> maybe "unset" pretty (format c) <> "\n"
      <> "  failure threshold: "
      <> maybe "unset" pretty (failThreshold c) <> "\n"
      <> indent 2
          ( showRulelist "error" (errorRules c)
              <> showRulelist "warning" (warningRules c)
              <> showRulelist "info" (infoRules c)
              <> showRulelist "style" (styleRules c)
              <> showRulelist "ignore" (ignoreRules c)
          )
      <> "strict labels: " <> maybe "unset" pretty (strictLabels c) <> "\n"
      <> prettyPrintLabelSchema (labelSchema c) <> "\n"
      <> prettyPrintRegistries (allowedRegistries c)

showRulelist :: String -> [RuleCode] -> Doc ann
showRulelist _ [] = ""
showRulelist name lst =
  ("override " <> pretty name <> ":\n") <> prettyPrintList pretty lst

-- | This function needs to convert the set to a list because Doc ann is not
-- ordered.
prettyPrintRegistries :: Set.Set Registry -> Doc ann
prettyPrintRegistries regs =
  if Set.null regs
  then ""
  else "allowed registries:\n"
          <> indent 2
              ( prettyPrintList
                  (\r -> " - " <> pretty (unRegistry r))
                  (Set.toList regs)
              )

prettyPrintLabelSchema :: LabelSchema -> Doc ann
prettyPrintLabelSchema ls =
  if Map.null ls
  then ""
  else "label schema:\n"
          <> indent 2
              ( prettyPrintList
                  (\(n, t) -> pretty n <> ": " <> pretty t)
                  (Map.toList ls)
              )

-- | pretty print a list with a custom pretty printing function for each element
prettyPrintList :: (a -> Doc ann) -> [a] -> Doc ann
prettyPrintList prnt lst =
  foldl (<>) "" (fmap (\i -> " - " <> prnt i <> "\n") lst)

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
