module Hadolint.Config.ConfigurationSpec where

import Data.Default
import Hadolint
import Test.Hspec

spec :: SpecWith ()
spec = do
  describe "Configuration" $ do
    it "override default configuration with empty config" $ do
      applyPartialConfiguration def mempty
        `shouldBe` def

    it "override default with specific configuration: no-fail" $ do
      let config = def { partialNoFail = Just True }
      applyPartialConfiguration def config
        `shouldBe` def { noFail = True }

    it "override default with specific configuration: no-color" $ do
      let config = def { partialNoColor = Just True }
      applyPartialConfiguration def config
        `shouldBe` def { noColor = True }

    it "empty should not override: no-color" $ do
      let config = def { partialNoColor = Just True }
          config2 = def
      applyPartialConfiguration def (config <> config2)
        `shouldBe` def { noColor = True }

    it "override default with specific configuration: verbose" $ do
      let config = def { partialVerbose = Just True }
      applyPartialConfiguration def config
        `shouldBe` def { verbose = True }

    it "override default with specific configuration: output-format json" $ do
      let config = def { partialFormats = [Json] }
      applyPartialConfiguration def config
        `shouldBe` def { formats = [Json] }

    -- Regression test for #1285: `--format sarif` on the command line
    -- together with `format: sarif` in the config file used to emit two
    -- concatenated SARIF documents, because the formats were merged
    -- additively. Each format is emitted only once.
    it "deduplicates the output format when declared twice" $ do
      let fromConfigfile = def { partialFormats = [Sarif] }
          fromCommandline = def { partialFormats = [Sarif] }
      applyPartialConfiguration def (fromConfigfile <> fromCommandline)
        `shouldBe` def { formats = [Sarif] }

    it "keeps distinct output formats when declared together" $ do
      let fromConfigfile = def { partialFormats = [Json] }
          fromCommandline = def { partialFormats = [Sarif] }
      applyPartialConfiguration def (fromConfigfile <> fromCommandline)
        `shouldBe` def { formats = [Json, Sarif] }

    it "override default with specific configuration: disable ignore pragma" $ do
      let config = def { partialDisableIgnorePragma = Just True }
      applyPartialConfiguration def config
        `shouldBe` def { disableIgnorePragma = True }
