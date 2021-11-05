module Hadolint.Config.ConfigurationSpec where

import Data.Default
import qualified Data.Map as Map
import qualified Data.Set as Set
import Hadolint
import Test.Hspec

spec :: SpecWith ()
spec = do
  describe "Configuration" $ do
    it "default configuration" $ do
      def
        `shouldBe` Configuration
          False
          False
          False
          TTY
          mempty
          mempty
          mempty
          mempty
          mempty
          mempty
          mempty
          False
          DLInfoC

    it "override default configuration with empty config" $ do
      applyPartialConfiguration def mempty
        `shouldBe` Configuration
          False
          False
          False
          TTY
          mempty
          mempty
          mempty
          mempty
          mempty
          mempty
          mempty
          False
          DLInfoC

    it "override default with specific configuration: no-fail" $ do
      let config =
            PartialConfiguration
              (Just True)
              Nothing
              Nothing
              mempty
              mempty
              mempty
              mempty
              mempty
              mempty
              mempty
              mempty
              Nothing
              mempty
      applyPartialConfiguration def config
        `shouldBe` Configuration
          True
          False
          False
          TTY
          mempty
          mempty
          mempty
          mempty
          mempty
          mempty
          mempty
          False
          DLInfoC

    it "override default with specific configuration: no-color" $ do
      let config =
            PartialConfiguration
              Nothing
              (Just True)
              Nothing
              mempty
              mempty
              mempty
              mempty
              mempty
              mempty
              mempty
              mempty
              Nothing
              mempty
      applyPartialConfiguration def config
        `shouldBe` Configuration
          False
          True
          False
          TTY
          mempty
          mempty
          mempty
          mempty
          mempty
          mempty
          mempty
          False
          DLInfoC

    it "empty should not override: no-color" $ do
      let config =
            PartialConfiguration
              Nothing
              (Just True)
              Nothing
              mempty
              mempty
              mempty
              mempty
              mempty
              mempty
              mempty
              mempty
              Nothing
              mempty
          config2 =
            PartialConfiguration
              Nothing
              Nothing
              Nothing
              mempty
              mempty
              mempty
              mempty
              mempty
              mempty
              mempty
              mempty
              Nothing
              mempty
      applyPartialConfiguration def (config <> config2)
        `shouldBe` Configuration
          False
          True
          False
          TTY
          mempty
          mempty
          mempty
          mempty
          mempty
          mempty
          mempty
          False
          DLInfoC

    it "override default with specific configuration: verbose" $ do
      let config =
            PartialConfiguration
              Nothing
              Nothing
              (Just True)
              mempty
              mempty
              mempty
              mempty
              mempty
              mempty
              mempty
              mempty
              Nothing
              mempty
      applyPartialConfiguration def config
        `shouldBe` Configuration
          False
          False
          True
          TTY
          mempty
          mempty
          mempty
          mempty
          mempty
          mempty
          mempty
          False
          DLInfoC

    it "override default with specific configuration: output-format json" $ do
      let config =
            PartialConfiguration
              Nothing
              Nothing
              Nothing
              (Just Json)
              mempty
              mempty
              mempty
              mempty
              mempty
              mempty
              mempty
              Nothing
              mempty
      applyPartialConfiguration def config
        `shouldBe` Configuration
          False
          False
          False
          Json
          mempty
          mempty
          mempty
          mempty
          mempty
          mempty
          mempty
          False
          DLInfoC
