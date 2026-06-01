module Hadolint.Formatter
  ( OutputFormat (..),
    Result (..),
    write,
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
import qualified Hadolint.Formatter.JUnit as FormatJUnit
import qualified Hadolint.Formatter.Json as FormatJson
import qualified Hadolint.Formatter.Sarif as FormatSarif
import qualified Hadolint.Formatter.SonarQube as FormatSonarQube
import qualified Hadolint.Formatter.TTY as FormatTTY
import System.IO


hWrite :: Foldable f =>
  Handle ->
  OutputFormat ->
  Bool ->
  Maybe FilePath ->
  f (Result Text DockerfileError) ->
  IO ()
hWrite handle format nocolor filePathInReport allResults =
  case format of
    Checkstyle -> FormatCheckstyle.hWrite handle allResults filePathInReport
    Codacy -> FormatCodacy.hWrite handle allResults
    CodeclimateJson -> FormatCodeclimate.hWrite handle allResults filePathInReport
    GitLabCodeclimateJson -> FormatCodeclimate.hWriteGitLab handle allResults filePathInReport
    Gnu -> FormatGnu.hWrite handle allResults
    JUnit -> FormatJUnit.hWrite handle allResults filePathInReport
    Json -> FormatJson.hWrite handle allResults
    Sarif -> FormatSarif.hWrite handle allResults
    SonarQube -> FormatSonarQube.hWrite handle allResults filePathInReport
    TTY -> FormatTTY.hWrite handle allResults nocolor


write :: Foldable f =>
  [FilePath] ->
  [OutputFormat] ->
  Bool ->
  Maybe FilePath ->
  f (Result Text DockerfileError) ->
  IO ()
write paths formats nocolor filePathInReport allResults =
  mapM_ writePath ziplist
  where
    ziplist = zipWithDef "-" TTY paths formats
    writePath ( p, f ) =
      if p == "-" then hWrite stdout f nocolor filePathInReport allResults
      else withFile p WriteMode (\h -> hWrite h f nocolor filePathInReport allResults)


zipWithDef :: FilePath -> OutputFormat -> [FilePath] -> [OutputFormat] -> [(FilePath, OutputFormat)]
zipWithDef defP defF []     []     = [(defP, defF)]
zipWithDef defP _    []     [f]    = [(defP, f)]
zipWithDef _    defF [p]    []     = [(p, defF)]
zipWithDef _    _    [p]    [f]    = [(p, f)]
zipWithDef defP defF []     (f:fs) = (defP, f):zipWithDef defP defF [] fs
zipWithDef defP defF (p:ps) []     = (p, defF):zipWithDef defP defF ps []
zipWithDef defP defF (p:ps) (f:fs) = (p, f):zipWithDef defP defF ps fs
