{-# LANGUAGE OverloadedStrings #-}
module ConfigSpec where

import Test.HUnit
import Test.Hspec
import Control.Monad (unless)
import qualified Data.ByteString.Char8 as Bytes
import qualified Data.YAML as Yaml

import Hadolint.Config

tests :: SpecWith ()
tests =
    describe "Config" $ do
        it "Parses config with only ignores" $
            let configFile =
                    [ "ignored:"
                    , "- DL3000"
                    , "- SC1010"
                    ]
                expected = ConfigFile (Just ["DL3000", "SC1010"]) Nothing
            in assertConfig expected (Bytes.unlines configFile)

        it "Parses config with only trustedRegistries" $
            let configFile =
                    [ "trustedRegistries:"
                    , "- hub.docker.com"
                    , "- my.shady.xyz"
                    ]
                expected = ConfigFile Nothing (Just ["hub.docker.com", "my.shady.xyz"])
            in assertConfig expected (Bytes.unlines configFile)

        it "Parses full file" $
            let configFile =
                    [ "trustedRegistries:"
                    , "- hub.docker.com"
                    , ""
                    , "ignored:"
                    , "- DL3000"
                    ]
                expected = ConfigFile (Just ["DL3000"]) (Just ["hub.docker.com"])
            in assertConfig expected (Bytes.unlines configFile)

assertConfig :: HasCallStack => ConfigFile -> Bytes.ByteString -> Assertion
assertConfig config s =
    case Yaml.decode1Strict s of
        Left err ->
            assertFailure err
        Right result ->
            checkResult result
  where
    checkResult result =
        unless (result == config) $
            assertFailure ("Config \n\n" ++ show config  ++ "\n\n is not \n\n" ++ show result)
