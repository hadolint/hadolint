module Hadolint.Config.ConfigfileSpec (spec) where

import Hadolint
import Test.Hspec
import qualified Data.ByteString.Char8 as Bytes
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.YAML as Yaml


spec :: SpecWith ()
spec =
  describe "Config from Configfile" $ do
    it "parse empty config file" $ do
      let yaml = []
          conf = parseYaml yaml
      conf `shouldBe` Left "empty YAML stream"

    it "parse `no-fail: true`" $ do
      let yaml = ["no-fail: true"]
          conf = parseYaml yaml
      conf `shouldBe` Right mempty { partialNoFail = Just True }

    it "parse `no-fail: false`" $ do
      let yaml = ["no-fail: false"]
          conf = parseYaml yaml
      conf `shouldBe` Right mempty { partialNoFail = Just False }

    it "parse `no-color: true`" $ do
      let yaml = ["no-color: true"]
          conf = parseYaml yaml
      conf `shouldBe` Right mempty { partialNoColor = Just True }

    it "parse `no-color: false`" $ do
      let yaml = ["no-color: false"]
          conf = parseYaml yaml
      conf `shouldBe` Right mempty { partialNoColor = Just False }

    it "parse `verbose: true`" $ do
      let yaml = ["verbose: true"]
          conf = parseYaml yaml
      conf `shouldBe` Right mempty { partialVerbose = Just True }

    it "parse `verbose: false`" $ do
      let yaml = ["verbose: false"]
          conf = parseYaml yaml
      conf `shouldBe` Right mempty { partialVerbose = Just False }

    it "parse `format: json`" $ do
      let yaml = ["format: json"]
          conf = parseYaml yaml
      conf `shouldBe` Right mempty { partialFormat = Just Json }

    it "parse `format: sarif`" $ do
      let yaml = ["format: sarif"]
          conf = parseYaml yaml
      conf `shouldBe` Right mempty { partialFormat = Just Sarif }

    it "parse override error rules" $ do
      let yaml = [ "override:",
                   "  error:",
                   "    - DL3020",
                   "    - DL3040",
                   "    - SC1020"
                 ]
          conf = parseYaml yaml
      conf `shouldBe` Right
                        mempty
                          { partialErrorRules = ["DL3020", "DL3040", "SC1020"] }

    it "parse override warning rules" $ do
      let yaml = [ "override:",
                   "  warning:",
                   "    - DL3020",
                   "    - DL3040",
                   "    - SC1020"
                 ]
          conf = parseYaml yaml
      conf `shouldBe` Right
                        mempty
                          { partialWarningRules = ["DL3020", "DL3040", "SC1020"]
                          }

    it "parse override info rules" $ do
      let yaml = [ "override:",
                   "  info:",
                   "    - DL3020",
                   "    - DL3040",
                   "    - SC1020"
                 ]
          conf = parseYaml yaml
      conf `shouldBe` Right
                        mempty
                          { partialInfoRules = ["DL3020", "DL3040", "SC1020"] }

    it "parse override style rules" $ do
      let yaml = [ "override:",
                   "  style:",
                   "    - DL3020",
                   "    - DL3040",
                   "    - SC1020"
                 ]
          conf = parseYaml yaml
      conf `shouldBe` Right
                        mempty
                          { partialStyleRules = ["DL3020", "DL3040", "SC1020"] }

    it "parse ignored rules" $ do
      let yaml = [ "ignored:",
                   "  - DL3020",
                   "  - DL3040",
                   "  - SC1020"
                 ]
          conf = parseYaml yaml
      conf `shouldBe` Right
                        mempty
                          { partialIgnoreRules = ["DL3020", "DL3040", "SC1020"] }

    it "parse trusted registries" $ do
      let yaml = [ "trustedRegistries:",
                   "  - foobar.com",
                   "  - barfoo.com"
                 ]
          conf = parseYaml yaml
      conf `shouldBe` Right
                        mempty
                          { partialAllowedRegistries =
                              Set.fromList ["foobar.com", "barfoo.com"]
                          }

    it "parse label schema" $ do
      let yaml = [ "label-schema:",
                   "  foo: email",
                   "  bar: text"
                 ]
          conf = parseYaml yaml
      conf `shouldBe` Right
                        mempty
                          { partialLabelSchema =
                              Map.fromList [("foo", Email), ("bar", RawText)]
                          }

    it "parse strict-labels: true" $ do
      let yaml = [ "strict-labels: true" ]
          conf = parseYaml yaml
      conf `shouldBe` Right mempty { partialStrictLabels = Just True }

    it "parse strict-labels: false" $ do
      let yaml = [ "strict-labels: false" ]
          conf = parseYaml yaml
      conf `shouldBe` Right mempty { partialStrictLabels = Just False }

    it "parse disable-ignore-pragma: true" $ do
      let yaml = [ "disable-ignore-pragma: true" ]
          conf = parseYaml yaml
      conf `shouldBe` Right mempty { partialDisableIgnorePragma = Just True }

    it "parse disable-ignore-pragma: false" $ do
      let yaml = [ "disable-ignore-pragma: false" ]
          conf = parseYaml yaml
      conf `shouldBe` Right mempty { partialDisableIgnorePragma = Just False }

    it "parse `failure-threshold: warning`" $ do
      let yaml = ["failure-threshold: warning"]
          conf = parseYaml yaml
      conf `shouldBe` Right mempty { partialFailureThreshold = Just DLWarningC }

    it "parse `failure-threshold: style`" $ do
      let yaml = ["failure-threshold: style"]
          conf = parseYaml yaml
      conf `shouldBe` Right mempty { partialFailureThreshold = Just DLStyleC }

-- Helper functions for parsing config files

parseYaml :: [Bytes.ByteString] -> Either String PartialConfiguration
parseYaml ls =
  case (Yaml.decode1Strict . Bytes.unlines) ls of
    Left (_, err) -> Left err
    Right res -> Right res
