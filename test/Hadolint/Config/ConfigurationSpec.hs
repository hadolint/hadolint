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
      let config = def { partialFormat = Just Json }
      applyPartialConfiguration def config
        `shouldBe` def { format = Json }

    it "override default with specific configuration: disable ignore pragma" $ do
      let config = def { partialDisableIgnorePragma = Just True }
      applyPartialConfiguration def config
        `shouldBe` def { disableIgnorePragma = True }
