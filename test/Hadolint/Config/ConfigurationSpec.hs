module Hadolint.Config.ConfigurationSpec where

import Data.Default
import Hadolint
import Test.Hspec
import qualified Data.Map as Map
import qualified Data.Set as Set


spec :: SpecWith ()
spec = do
  describe "Configuration" $ do
    it "default configuration" $ do
      def `shouldBe` Configuration
                        (Just False)
                        (Just False)
                        (Just False)
                        (Just TTY)
                        ( LintOptions
                            mempty
                            mempty
                            mempty
                            mempty
                            mempty
                            ( RulesConfig
                                Set.empty
                                Map.empty
                                (Just False)
                            )
                        )
                        (Just DLInfoC)

    it "override default with specific configuration: no-fail" $ do
      let config = Configuration
                     (Just True)
                     Nothing
                     Nothing
                     mempty
                     mempty
                     mempty
      (def <> config) `shouldBe` Configuration
                                    (Just True)
                                    (Just False)
                                    (Just False)
                                    (Just TTY)
                                    def
                                    (Just DLInfoC)

    it "override default with specific configuration: no-color" $ do
      let config = Configuration
                     Nothing
                     (Just True)
                     Nothing
                     mempty
                     mempty
                     mempty
      (def <> config) `shouldBe` Configuration
                                    (Just False)
                                    (Just True)
                                    (Just False)
                                    (Just TTY)
                                    def
                                    (Just DLInfoC)

    it "empty should not override: no-color" $ do
      let config = Configuration
                     Nothing
                     (Just True)
                     Nothing
                     mempty
                     mempty
                     mempty
          config2 = Configuration
                      Nothing
                      Nothing
                      Nothing
                      mempty
                      mempty
                      mempty
      (def <> config <> config2) `shouldBe` Configuration
                                              (Just False)
                                              (Just True)
                                              (Just False)
                                              (Just TTY)
                                              def
                                              (Just DLInfoC)

    it "override default with specific configuration: verbose" $ do
      let config = Configuration
                     Nothing
                     Nothing
                     (Just True)
                     mempty
                     mempty
                     mempty
      (def <> config) `shouldBe` Configuration
                                    (Just False)
                                    (Just False)
                                    (Just True)
                                    (Just TTY)
                                    def
                                    (Just DLInfoC)

    it "override default with specific configuration: output-format json" $ do
      let config = Configuration
                     Nothing
                     Nothing
                     Nothing
                     (Just Json)
                     mempty
                     mempty
      (def <> config) `shouldBe` Configuration
                                    (Just False)
                                    (Just False)
                                    (Just False)
                                    (Just Json)
                                    def
                                    (Just DLInfoC)
