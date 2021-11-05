module Hadolint.Formatter.SarifSpec (spec) where

import Helpers
import Hadolint
  ( CheckFailure (..),
    DLSeverity (..),
    Result(..),
    printResults,
    OutputFormat (..),
    getShortVersion,
  )
import Test.Hspec


spec :: SpecWith ()
spec = do
  let ?noColor = True

  describe "Formatter: Sarif" $ do
    it "print empty results" $ do
      let checkFails = []
          expectation = "{\"runs\":[{\"defaultSourceLanguage\":\"dockerfi\
                        \le\",\"results\":[],\"tool\":{\"driver\":{\"full\
                        \Name\":\"Haskell Dockerfile Linter\",\"downloadU\
                        \ri\":\"https://github.com/hadolint/hadolint\",\"\
                        \shortDescription\":{\"text\":\"Dockerfile linter\
                        \, validate inline bash, written in Haskell\"},\"\
                        \name\":\"Hadolint\",\"version\":\""
                        ++ getShortVersion
                        ++ "\"}}}],\"version\":\"2.1.0\",\"$schema\
                           \\":\"http://json.schemastore.org/sarif-2.1.0\"}"
      assertFormatter Sarif checkFails expectation
