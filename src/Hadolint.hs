module Hadolint
  ( module Hadolint.Lint,
    module Hadolint.Process,
    module Hadolint.Config,
    Result (..),
    OutputFormat (..),
    shallSkipErrorStatus,
    printResults,
  )
where

import Data.Text (Text)
import Hadolint.Config
import qualified Hadolint.Formatter.Checkstyle
import qualified Hadolint.Formatter.Codacy
import qualified Hadolint.Formatter.Codeclimate
import Hadolint.Formatter.Format (Result (..))
import qualified Hadolint.Formatter.Json
import qualified Hadolint.Formatter.TTY
import Hadolint.Lint
import Hadolint.Process
import Language.Docker.Parser (DockerfileError)

data OutputFormat
  = Json
  | TTY
  | CodeclimateJson
  | GitlabCodeclimateJson
  | Checkstyle
  | Codacy
  deriving (Show, Eq)

shallSkipErrorStatus :: OutputFormat -> Bool
shallSkipErrorStatus format = format `elem` [CodeclimateJson, Codacy]

printResults :: Foldable f => OutputFormat -> Bool -> f (Result Text DockerfileError) -> IO ()
printResults format nocolor allResults =
  case format of
    TTY -> Hadolint.Formatter.TTY.printResults allResults nocolor
    Json -> Hadolint.Formatter.Json.printResults allResults
    Checkstyle -> Hadolint.Formatter.Checkstyle.printResults allResults
    CodeclimateJson -> Hadolint.Formatter.Codeclimate.printResults allResults
    GitlabCodeclimateJson -> Hadolint.Formatter.Codeclimate.printGitlabResults allResults
    Codacy -> Hadolint.Formatter.Codacy.printResults allResults
