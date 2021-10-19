module Hadolint.Lint
  ( lintIO,
    lint,
    analyze,
    LintOptions (..),
    TrustedRegistry,
  )
where

import Data.Default
import Data.Text (Text)
import Data.Text.Prettyprint.Doc (Pretty, pretty, Doc)
import Hadolint.Rule (RuleCode, DLSeverity (..))
import Language.Docker.Parser (DockerfileError, Error)
import Language.Docker.Syntax (Dockerfile)
import qualified Control.Parallel.Strategies as Parallel
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Hadolint.Formatter.Format as Format
import qualified Hadolint.Process
import qualified Hadolint.Rule
import qualified Language.Docker as Docker

type TrustedRegistry = Text

data LintOptions = LintOptions
  { errorRules :: [RuleCode],
    warningRules :: [RuleCode],
    infoRules :: [RuleCode],
    styleRules :: [RuleCode],
    ignoreRules :: [RuleCode],
    rulesConfig :: Hadolint.Process.RulesConfig
  }
  deriving (Eq, Show)

instance Pretty LintOptions where
  pretty o =
    showRulelist "error" (errorRules o)
      <> showRulelist "warning" (warningRules o)
      <> showRulelist "info" (infoRules o)
      <> showRulelist "style" (styleRules o)
      <> showRulelist "ignore" (ignoreRules o)
      <> pretty (rulesConfig o)

showRulelist :: String -> [RuleCode] -> Doc ann
showRulelist _ [] = ""
showRulelist name list =
  foldl
    (<>)
    ""
    ( ("override " <> pretty name <> ":\n")
        : fmap (\i -> " - " <> pretty i <> "\n") list
    )


instance Semigroup LintOptions where
  LintOptions a1 a2 a3 a4 a5 a6 <> LintOptions b1 b2 b3 b4 b5 b6 =
    LintOptions
      (a1 <> b1)
      (a2 <> b2)
      (a3 <> b3)
      (a4 <> b4)
      (a5 <> b5)
      (a6 <> b6)

instance Monoid LintOptions where
  mempty = LintOptions mempty mempty mempty mempty mempty mempty

instance Default LintOptions where
  def = LintOptions mempty mempty mempty mempty mempty def

-- | Performs the process of parsing the dockerfile and analyzing it with all
-- the applicable rules, depending on the list of ignored rules.
lintIO ::
  LintOptions ->
  NonEmpty.NonEmpty FilePath ->
  IO (NonEmpty.NonEmpty (Format.Result Text DockerfileError))
lintIO options dFiles = do
  parsedFiles <- mapM parseFile (NonEmpty.toList dFiles)
  return $ NonEmpty.fromList (lint options parsedFiles)
  where
    parseFile :: String -> IO (Text, Either Error Dockerfile)
    parseFile "-" = do
      res <- Docker.parseStdin
      return (Text.pack "-", res)
    parseFile s = do
      res <- Docker.parseFile s
      return (Text.pack s, res)

lint ::
  LintOptions ->
  [(Text, Either Error Dockerfile)] ->
  [Format.Result Text DockerfileError]
lint options parsedFiles = gather results `Parallel.using` parallelRun
  where
    gather = fmap (uncurry Format.toResult)
    results =
      [ ( name,
          fmap (analyze options) parseResult
        )
        | (name, parseResult) <- parsedFiles
      ]
    parallelRun = Parallel.parList Parallel.rseq

analyze :: LintOptions -> Dockerfile -> Seq.Seq Hadolint.Rule.CheckFailure
analyze options dockerfile = fixer process
  where
    fixer = fixSeverity options
    process = Hadolint.Process.run (rulesConfig options) dockerfile

fixSeverity ::
  LintOptions ->
  Seq.Seq Hadolint.Rule.CheckFailure ->
  Seq.Seq Hadolint.Rule.CheckFailure
fixSeverity LintOptions {..} =
  Seq.filter ignoredRules . Seq.mapWithIndex (const correctSeverity)
  where
    correctSeverity =
      makeSeverity Hadolint.Rule.DLErrorC errorRules
        . makeSeverity Hadolint.Rule.DLWarningC warningRules
        . makeSeverity Hadolint.Rule.DLInfoC infoRules
        . makeSeverity Hadolint.Rule.DLStyleC styleRules

    ignoredRules = ignoreFilter ignoreRules

    makeSeverity s rules rule@Hadolint.Rule.CheckFailure {code} =
      if code `elem` rules
        then rule {Hadolint.Rule.severity = s}
        else rule

    ignoreFilter :: [RuleCode] -> Hadolint.Rule.CheckFailure -> Bool
    ignoreFilter ignored Hadolint.Rule.CheckFailure {code, severity} =
      code `notElem` ignored && severity /= Hadolint.Rule.DLIgnoreC
