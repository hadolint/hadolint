module Hadolint.Formatter
  ( OutputFormat (..),
    Result (..),
    printResults,
    readMaybeOutputFormat,
  )
where

import Data.Text (Text)
import Hadolint.Formatter.Format
import Language.Docker.Parser (DockerfileError)
import qualified Hadolint.Formatter.Checkstyle as FormatCheckstyle
import qualified Hadolint.Formatter.Codacy as FormatCodacy
import qualified Hadolint.Formatter.Codeclimate as FormatCodeclimate
import qualified Hadolint.Formatter.Gnu as FormatGnu
import qualified Hadolint.Formatter.Json as FormatJson
import qualified Hadolint.Formatter.Sarif as FormatSarif
import qualified Hadolint.Formatter.SonarQube as FormatSonarQube
import qualified Hadolint.Formatter.TTY as FormatTTY


printResults ::
  Foldable f =>
  OutputFormat ->
  Bool ->
  Maybe FilePath ->
  f (Result Text DockerfileError) ->
  IO ()
printResults format nocolor filePathInReport allResults =
  case format of
    Checkstyle -> FormatCheckstyle.printResults allResults filePathInReport
    Codacy -> FormatCodacy.printResults allResults
    CodeclimateJson -> FormatCodeclimate.printResults allResults
    GitLabCodeclimateJson -> FormatCodeclimate.printGitLabResults allResults
    Gnu -> FormatGnu.printResults allResults
    Json -> FormatJson.printResults allResults
    Sarif -> FormatSarif.printResults allResults
    SonarQube -> FormatSonarQube.printResults allResults
    TTY -> FormatTTY.printResults allResults nocolor
