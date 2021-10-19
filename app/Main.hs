module Main where

import Control.Monad (when)
import Data.Default
import Data.Maybe
import Hadolint (OutputFormat (..), printResults, DLSeverity (..))
import Hadolint.Config
import Prettyprinter
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Sequence as Seq
import qualified Hadolint
import qualified Hadolint.Rule as Rule
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


noFailure :: Hadolint.Result s e -> Maybe DLSeverity -> Bool
noFailure (Hadolint.Result _ Seq.Empty Seq.Empty) _ = True
noFailure (Hadolint.Result _ Seq.Empty fails) (Just cutoff) =
  Seq.null (Seq.filter (\f -> Rule.severity f <= cutoff) fails)
noFailure _ _ = False

exitProgram ::
  Foldable f =>
  Configuration ->
  f (Hadolint.Result s e) ->
  IO ()
exitProgram conf res
  | Just True == noFail conf = exitSuccess
  | Just CodeclimateJson == format conf = exitSuccess
  | Just Codacy == format conf = exitSuccess
  | all (`noFailure` failThreshold conf) res = exitSuccess
  | otherwise = exitFailure

runLint ::
  CommandlineConfig ->
  Configuration ->
  IO ()
runLint cmd conf = do
  let files = NonEmpty.fromList $ dockerfiles cmd
      filePathInReport = filePathInReportOption cmd
  res <- Hadolint.lintIO (lintingOptions conf) files
  printResults
    (fromMaybe def $ format conf)  -- not pretty but works
    (Just True == noColor conf)
    filePathInReport
    res
  exitProgram conf res

execute :: CommandlineConfig -> Configuration -> IO ()
execute CommandlineConfig {showVersion = True} _ =
  putStrLn Hadolint.getVersion >> exitSuccess
execute CommandlineConfig {dockerfiles = []} _ =
  putStrLn "Please provide a Dockerfile" >> exitFailure
execute cmd config = runLint cmd config


main :: IO ()
main = do
  invokedWith <- execParser opts
  fromEnvironment <- getConfigFromEnvironment
  let fromCommandline = configuration invokedWith
  eitherFromConfigfile <- getConfigFromFile
    (configFile invokedWith) (Just True == verbose fromCommandline)
  fromConfigfile <- getConfigFromEither eitherFromConfigfile

  let runningConfig =
        def <> fromEnvironment <> fromConfigfile <> fromCommandline

  when (Just True == verbose fromCommandline)
    (hPrint stderr (pretty runningConfig))
  execute invokedWith runningConfig
  where
    opts =
      info
        ( helper <*> parseCommandline )
        ( fullDesc
            <> header "hadolint - Dockerfile Linter written in Haskell"
            <> progDesc "Lint Dockerfile for errors and best practices"
        )

    -- Either return the config or print the error message
    getConfigFromEither ei =
      case ei of
        Left err -> do
          hPrint stderr err
          return mempty
        Right conf -> return conf
