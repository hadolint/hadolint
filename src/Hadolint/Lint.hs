module Hadolint.Lint
  ( lintIO,
    lint,
    analyze,
    TrustedRegistry,
  )
where

import Data.Text (Text)
import Hadolint.Config.Configuration (Configuration (..))
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


-- | Performs the process of parsing the dockerfile and analyzing it with all
-- the applicable rules, depending on the list of ignored rules.
lintIO ::
  Configuration ->
  NonEmpty.NonEmpty FilePath ->
  IO (NonEmpty.NonEmpty (Format.Result Text DockerfileError))
lintIO config dFiles = do
  parsedFiles <- mapM parseFile (NonEmpty.toList dFiles)
  return $ NonEmpty.fromList (lint config parsedFiles)
  where
    parseFile :: String -> IO (Text, Either Error Dockerfile)
    parseFile "-" = do
      res <- Docker.parseStdin
      return (Text.pack "-", res)
    parseFile s = do
      res <- Docker.parseFile s
      return (Text.pack s, res)

lint ::
  Configuration ->
  [(Text, Either Error Dockerfile)] ->
  [Format.Result Text DockerfileError]
lint config parsedFiles = gather results `Parallel.using` parallelRun
  where
    gather = fmap (uncurry Format.toResult)
    results =
      [ ( name,
          fmap (analyze config) parseResult
        )
        | (name, parseResult) <- parsedFiles
      ]
    parallelRun = Parallel.parList Parallel.rseq

analyze :: Configuration -> Dockerfile -> Seq.Seq Hadolint.Rule.CheckFailure
analyze config dockerfile = fixer process
  where
    fixer = fixSeverity config
    process = Hadolint.Process.run config dockerfile

fixSeverity ::
  Configuration ->
  Seq.Seq Hadolint.Rule.CheckFailure ->
  Seq.Seq Hadolint.Rule.CheckFailure
fixSeverity Configuration {..} =
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
