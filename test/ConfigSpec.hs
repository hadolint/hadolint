{-# LANGUAGE OverloadedStrings #-}

module ConfigSpec where

import Control.Monad (unless)
import qualified Data.ByteString.Char8 as Bytes
import qualified Data.YAML as Yaml
import Hadolint.Config
import Test.HUnit
import Test.Hspec

tests :: SpecWith ()
tests =
  describe "Config" $ do
    it "Parses config with only error severities" $
      let configFile =
            [ "override:",
              "  error:",
              "    - DL3000",
              "    - SC1010"
            ]
          override = Just (OverrideConfig (Just ["DL3000", "SC1010"]) Nothing Nothing Nothing)
          expected = ConfigFile override Nothing Nothing
       in assertConfig expected (Bytes.unlines configFile)

    it "Parses config with only warning severities" $
      let configFile =
            [ "override:",
              "  warning:",
              "    - DL3000",
              "    - SC1010"
            ]
          override = Just (OverrideConfig Nothing (Just ["DL3000", "SC1010"]) Nothing Nothing)
          expected = ConfigFile override Nothing Nothing
       in assertConfig expected (Bytes.unlines configFile)

    it "Parses config with only info severities" $
      let configFile =
            [ "override:",
              "  info:",
              "    - DL3000",
              "    - SC1010"
            ]
          override = Just (OverrideConfig Nothing Nothing (Just ["DL3000", "SC1010"]) Nothing)
          expected = ConfigFile override Nothing Nothing
       in assertConfig expected (Bytes.unlines configFile)

    it "Parses config with only style severities" $
      let configFile =
            [ "override:",
              "  style:",
              "    - DL3000",
              "    - SC1010"
            ]
          override = Just (OverrideConfig Nothing Nothing Nothing (Just ["DL3000", "SC1010"]))
          expected = ConfigFile override Nothing Nothing
       in assertConfig expected (Bytes.unlines configFile)

    it "Parses config with only ignores" $
      let configFile =
            [ "ignored:",
              "- DL3000",
              "- SC1010"
            ]
          expected = ConfigFile Nothing (Just ["DL3000", "SC1010"]) Nothing
       in assertConfig expected (Bytes.unlines configFile)

    it "Parses config with only trustedRegistries" $
      let configFile =
            [ "trustedRegistries:",
              "- hub.docker.com",
              "- my.shady.xyz"
            ]
          expected = ConfigFile Nothing Nothing (Just ["hub.docker.com", "my.shady.xyz"])
       in assertConfig expected (Bytes.unlines configFile)

    it "Parses full file" $
      let configFile =
            [ "override:",
              "  info:",
              "    - DL3002",
              "  style:",
              "    - DL3004",
              "  warning:",
              "    - DL3003",
              "  error:",
              "    - DL3001",
              "trustedRegistries:",
              "- hub.docker.com",
              "",
              "",
              "ignored:",
              "- DL3000"
            ]
          override = Just (OverrideConfig (Just ["DL3001"]) (Just ["DL3003"]) (Just ["DL3002"]) (Just ["DL3004"]))
          expected = ConfigFile override (Just ["DL3000"]) (Just ["hub.docker.com"])
       in assertConfig expected (Bytes.unlines configFile)

assertConfig :: HasCallStack => ConfigFile -> Bytes.ByteString -> Assertion
assertConfig config s =
  case Yaml.decode1Strict s of
    Left (_, err) ->
      assertFailure err
    Right result ->
      checkResult result
  where
    checkResult result =
      unless (result == config) $
        assertFailure ("Config \n\n" ++ show config ++ "\n\n is not \n\n" ++ show result)
