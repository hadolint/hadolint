module Hadolint.Config.EnvironmentSpec (spec) where

import Hadolint
import System.Environment hiding (setEnv)
import System.Environment.Blank (setEnv)
import System.Info
import Test.Hspec
import qualified Data.Map as Map
import qualified Data.Set as Set


spec :: SpecWith ()
spec = do
  describe "Config from Environment" $ do
    withoutAll $ do
      it "Empty config when nothing is set" $ do
        conf <- getConfigFromEnvironment
        conf `shouldBe` mempty

    withJustEnv "HADOLINT_NOFAIL" "1" $ do
      it "parse HADOLINT_NOFAIL=1" $ do
        conf <- getConfigFromEnvironment
        conf `shouldBe` mempty { partialNoFail = Just True }

    withJustEnv "HADOLINT_NOFAIL" "0" $ do
      it "parse HADOLINT_NOFAIL=0" $ do
        conf <- getConfigFromEnvironment
        conf `shouldBe` mempty {partialNoFail = Just False }

    withJustEnv "NO_COLOR" "y" $ do
      it "parse NO_COLOR=y" $ do
        conf <- getConfigFromEnvironment
        conf `shouldBe` mempty { partialNoColor = Just True }

    withJustEnv "NO_COLOR" "n" $ do
      it "parse NO_COLOR=n" $ do
        conf <- getConfigFromEnvironment
        conf `shouldBe` mempty { partialNoColor = Just True }

    -- The expected value in this test changes based on the OS, but in order to
    -- respect the spec of https://no-color.org, an environment variable set to
    -- the empty string must still count as set, at least on platforms that
    -- support setting an environment variable to the empty string (POSIX).
    withJustEnv "NO_COLOR" "" $ do
      it "parse NO_COLOR set to empty string" $ do
        conf <- getConfigFromEnvironment
        conf `shouldBe` mempty { partialNoColor = expectedValueVarEmptyString }

    withoutAll $ do
      it "parse NO_COLOR unset" $ do
        conf <- getConfigFromEnvironment
        conf `shouldBe` mempty { partialNoColor = Nothing }

    withJustEnv "HADOLINT_VERBOSE" "on" $ do
      it "parse HADOLINT_VERBOSE=on" $ do
        conf <- getConfigFromEnvironment
        conf `shouldBe` mempty { partialVerbose = Just True }

    withJustEnv "HADOLINT_VERBOSE" "off" $ do
      it "parse HADOLINT_VERBOSE=off" $ do
        conf <- getConfigFromEnvironment
        conf `shouldBe` mempty { partialVerbose = Just False }

    withJustEnv "HADOLINT_FORMAT" "json" $ do
      it "parse HADOLINT_FORMAT=json" $ do
        conf <- getConfigFromEnvironment
        conf `shouldBe` mempty { partialFormat = Just Json }

    withJustEnv "HADOLINT_FORMAT" "sarif" $ do
      it "parse HADOLINT_FORMAT=sarif" $ do
        conf <- getConfigFromEnvironment
        conf `shouldBe` mempty { partialFormat = Just Sarif }

    withJustEnv "HADOLINT_OVERRIDE_ERROR" "DL3010" $ do
      it "parse HADOLINT_OVERRIDE_ERROR=DL3010" $ do
        conf <- getConfigFromEnvironment
        conf `shouldBe` mempty { partialErrorRules = ["DL3010"] }

    withJustEnv "HADOLINT_OVERRIDE_ERROR" "DL3010,DL3011" $ do
      it "parse HADOLINT_OVERRIDE_ERROR=DL3010,DL3011" $ do
        conf <- getConfigFromEnvironment
        conf `shouldBe` mempty { partialErrorRules = ["DL3010", "DL3011"] }

    withJustEnv "HADOLINT_OVERRIDE_WARNING" "DL3010" $ do
      it "parse HADOLINT_OVERRIDE_WARNING=DL3010" $ do
        conf <- getConfigFromEnvironment
        conf `shouldBe` mempty { partialWarningRules = ["DL3010"] }

    withJustEnv "HADOLINT_OVERRIDE_WARNING" "DL3010,DL3011" $ do
      it "parse HADOLINT_OVERRIDE_WARNING=DL3010,DL3011" $ do
        conf <- getConfigFromEnvironment
        conf `shouldBe` mempty { partialWarningRules = ["DL3010", "DL3011"] }

    withJustEnv "HADOLINT_OVERRIDE_INFO" "DL3010" $ do
      it "parse HADOLINT_OVERRIDE_INFO=DL3010" $ do
        conf <- getConfigFromEnvironment
        conf `shouldBe` mempty { partialInfoRules = ["DL3010"] }

    withJustEnv "HADOLINT_OVERRIDE_INFO" "DL3010,DL3011" $ do
      it "parse HADOLINT_OVERRIDE_INFO=DL3010,DL3011" $ do
        conf <- getConfigFromEnvironment
        conf `shouldBe` mempty { partialInfoRules = ["DL3010", "DL3011"] }

    withJustEnv "HADOLINT_OVERRIDE_STYLE" "DL3010" $ do
      it "parse HADOLINT_OVERRIDE_STYLE=DL3010" $ do
        conf <- getConfigFromEnvironment
        conf `shouldBe` mempty { partialStyleRules = ["DL3010"] }

    withJustEnv "HADOLINT_OVERRIDE_STYLE" "DL3010,DL3011" $ do
      it "parse HADOLINT_OVERRIDE_STYLE=DL3010,DL3011" $ do
        conf <- getConfigFromEnvironment
        conf `shouldBe` mempty { partialStyleRules = ["DL3010", "DL3011"] }

    withJustEnv "HADOLINT_IGNORE" "DL3010" $ do
      it "parse HADOLINT_IGNORE=DL3010" $ do
        conf <- getConfigFromEnvironment
        conf `shouldBe` mempty { partialIgnoreRules = ["DL3010"] }

    withJustEnv "HADOLINT_IGNORE" "DL3010,DL3011" $ do
      it "parse HADOLINT_IGNORE=DL3010,DL3011" $ do
        conf <- getConfigFromEnvironment
        conf `shouldBe` mempty { partialIgnoreRules = ["DL3010", "DL3011"] }

    withJustEnv "HADOLINT_TRUSTED_REGISTRIES" "foobar.com" $ do
      it "parse HADOLINT_TRUSTED_REGISTRIES=foobar.com" $ do
        conf <- getConfigFromEnvironment
        conf `shouldBe` mempty
                          { partialAllowedRegistries =
                              Set.fromList ["foobar.com"]
                          }

    withJustEnv "HADOLINT_TRUSTED_REGISTRIES" "foobar.com,barfoo.com" $ do
      it "parse HADOLINT_TRUSTED_REGISTRIES=foobar.com,barfoo.com" $ do
        conf <- getConfigFromEnvironment
        conf `shouldBe` mempty
                          { partialAllowedRegistries =
                              Set.fromList ["foobar.com", "barfoo.com"]
                          }

    withJustEnv "HADOLINT_REQUIRE_LABELS" "foo:email" $ do
      it "parse HADOLINT_REQUIRE_LABELS=foo:email" $ do
        conf <- getConfigFromEnvironment
        conf `shouldBe` mempty
                          { partialLabelSchema = Map.fromList [("foo", Email)] }

    withJustEnv "HADOLINT_REQUIRE_LABELS" "foo:email,bar:text" $ do
      it "parse HADOLINT_REQUIRE_LABELS=foo:email,bar:text" $ do
        conf <- getConfigFromEnvironment
        conf `shouldBe` mempty
                          { partialLabelSchema =
                              Map.fromList [("foo", Email), ("bar", RawText)]
                          }

    withJustEnv "HADOLINT_STRICT_LABELS" "yes" $ do
      it "parse HADOLINT_STRICT_LABELS=yes" $ do
        conf <- getConfigFromEnvironment
        conf `shouldBe` mempty { partialStrictLabels = Just True }

    withJustEnv "HADOLINT_STRICT_LABELS" "false" $ do
      it "parse HADOLINT_STRICT_LABELS=false" $ do
        conf <- getConfigFromEnvironment
        conf `shouldBe` mempty { partialStrictLabels = Just False }

    withJustEnv "HADOLINT_DISABLE_IGNORE_PRAGMA" "yes" $ do
      it "parse HADOLINT_DISABLE_IGNORE_PRAGMA=yes" $ do
        conf <- getConfigFromEnvironment
        conf `shouldBe` mempty { partialDisableIgnorePragma = Just True }

    withJustEnv "HADOLINT_DISABLE_IGNORE_PRAGMA" "false" $ do
      it "parse HADOLINT_DISABLE_IGNORE_PRAGMA=false" $ do
        conf <- getConfigFromEnvironment
        conf `shouldBe` mempty { partialDisableIgnorePragma = Just False }

    withJustEnv "HADOLINT_FAILURE_THRESHOLD" "error" $ do
      it "parse HADOLINT_FAILURE_THRESHOLD=error" $ do
        conf <- getConfigFromEnvironment
        conf `shouldBe` mempty { partialFailureThreshold = Just DLErrorC }

    withJustEnv "HADOLINT_FAILURE_THRESHOLD" "style" $ do
      it "parse HADOLINT_FAILURE_THRESHOLD=style" $ do
        conf <- getConfigFromEnvironment
        conf `shouldBe` mempty { partialFailureThreshold = Just DLStyleC }

-- Helper functions for setup and teardown of the environment

withoutAll :: SpecWith () -> SpecWith ()
withoutAll = before unsetAll

withJustEnv :: String -> String -> SpecWith () -> SpecWith ()
withJustEnv name value test =
  before_ (setJustEnv name value) $ after_ (unsetEnv name) test

setJustEnv :: String -> String -> IO ()
setJustEnv name value = do
  unsetAll
  setEnv name value False

unsetAll :: IO ()
unsetAll = do
  unsetEnv "HADOLINT_NOFAIL"
  unsetEnv "NO_COLOR"
  unsetEnv "HADOLINT_VERBOSE"
  unsetEnv "HADOLINT_FORMAT"
  unsetEnv "HADOLINT_OVERRIDE_ERROR"
  unsetEnv "HADOLINT_OVERRIDE_WARNING"
  unsetEnv "HADOLINT_OVERRIDE_INFO"
  unsetEnv "HADOLINT_OVERRIDE_STYLE"
  unsetEnv "HADOLINT_IGNORE"
  unsetEnv "HADOLINT_TRUSTED_REGISTRIES"
  unsetEnv "HADOLINT_REQUIRE_LABELS"
  unsetEnv "HADOLINT_STRICT_LABELS"
  unsetEnv "HADOLINT_FAILURE_THRESHOLD"
  unsetEnv "HADOLINT_DISABLE_IGNORE_PRAGMA"

-- On Windows setting an environment variable to the empty string is the same as
-- unsetting it. On POSIX, an environment variable can be set and contain the
-- empty string. In order to generate the correct expected value one must check
-- the operating system type.
expectedValueVarEmptyString :: Maybe Bool
expectedValueVarEmptyString
  | os == "mingw32" = Nothing
  | otherwise = Just True
