module Main where

import Control.Monad (when)
import Data.Default
import Data.Maybe
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Sequence as Seq
import Hadolint (OutputFormat (..), printResults)
import qualified Hadolint
import qualified Hadolint.Rule as Rule
import Hadolint.Config
import Options.Applicative
  ( execParser,
    fullDesc,
    header,
    helper,
    info,
    progDesc
  )
-- version from hadolint.cabal file
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPrint, stderr)


noFailure :: Hadolint.Result s e -> Rule.DLSeverity -> Bool
noFailure (Hadolint.Result _ Seq.Empty Seq.Empty) _ = True
noFailure (Hadolint.Result _ Seq.Empty fails) cutoff =
  Seq.null (Seq.filter (\f -> Rule.severity f <= cutoff) fails)
noFailure _ _ = False

exitProgram ::
  Foldable f =>
  Configuration ->
  f (Hadolint.Result s e) ->
  IO ()
exitProgram conf res
  | fromJust (noFail conf) = exitSuccess
  | shallSkipErrorStatus (fromJust $ format conf) = exitSuccess
  | all (`noFailure` fromJust (failThreshold conf)) res =
      exitSuccess
  | otherwise = exitFailure

runLint ::
  CommandlineConfig ->
  Configuration ->
  IO ()
runLint cmd conf = do
  let files = NonEmpty.fromList $ dockerfiles cmd
      filePathInReport = filePathInReportOption cmd
  res <- Hadolint.lintIO (lintingOptions conf) files
  printResults (fromJust $ format conf) (fromJust $ noColor conf) filePathInReport res
  exitProgram conf res


shallSkipErrorStatus :: OutputFormat -> Bool
shallSkipErrorStatus CodeclimateJson = True
shallSkipErrorStatus Codacy = True
shallSkipErrorStatus _ = False

main :: IO ()
main = do
  invokedWith <- execParser opts
  fromEnvironment <- getConfigFromEnvironment
  let fromCommandline = configuration invokedWith
  eitherFromConfigfile <- getConfigFromFile
    (configFile invokedWith) (Just True == verbose fromCommandline)
  fromConfigfile <- foobar eitherFromConfigfile

  let runningConfig =
        def <> fromEnvironment <> fromConfigfile <> fromCommandline

  when (Just True == verbose fromCommandline) (hPrint stderr runningConfig)
  execute invokedWith runningConfig
  where
    execute CommandlineConfig {showVersion = True} _ =
      putStrLn Hadolint.getVersion >> exitSuccess
    execute CommandlineConfig {dockerfiles = []} _ =
      putStrLn "Please provide a Dockerfile" >> exitFailure
    execute cmd config = runLint cmd config

    opts =
      info
        ( helper <*> parseCommandline )
        ( fullDesc <> progDesc "Lint Dockerfile for errors and best practices"
            <> header "hadolint - Dockerfile Linter written in Haskell"
        )

    foobar ei =
      case ei of
        Left err -> do
          hPrint stderr err
          return mempty
        Right conf -> return conf
