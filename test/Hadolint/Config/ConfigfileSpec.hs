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
      conf `shouldBe` Right mempty { noFail = Just True }

    it "parse `no-fail: false`" $ do
      let yaml = ["no-fail: false"]
          conf = parseYaml yaml
      conf `shouldBe` Right mempty { noFail = Just False }

    it "parse `no-color: true`" $ do
      let yaml = ["no-color: true"]
          conf = parseYaml yaml
      conf `shouldBe` Right mempty { noColor = Just True }

    it "parse `no-color: false`" $ do
      let yaml = ["no-color: false"]
          conf = parseYaml yaml
      conf `shouldBe` Right mempty { noColor = Just False }

    it "parse `verbose: true`" $ do
      let yaml = ["verbose: true"]
          conf = parseYaml yaml
      conf `shouldBe` Right mempty { verbose = Just True }

    it "parse `verbose: false`" $ do
      let yaml = ["verbose: false"]
          conf = parseYaml yaml
      conf `shouldBe` Right mempty { verbose = Just False }

    it "parse `output-format: json`" $ do
      let yaml = ["output-format: json"]
          conf = parseYaml yaml
      conf `shouldBe` Right mempty { format = Just Json }

    it "parse `output-format: sarif`" $ do
      let yaml = ["output-format: sarif"]
          conf = parseYaml yaml
      conf `shouldBe` Right mempty { format = Just Sarif }

    it "parse override error rules" $ do
      let yaml = [ "override:",
                   "  error:",
                   "    - DL3020",
                   "    - DL3040",
                   "    - SC1020"
                 ]
          conf = parseYaml yaml
      conf `shouldBe` Right
                        mempty { errorRules = ["DL3020", "DL3040", "SC1020"] }

    it "parse override warning rules" $ do
      let yaml = [ "override:",
                   "  warning:",
                   "    - DL3020",
                   "    - DL3040",
                   "    - SC1020"
                 ]
          conf = parseYaml yaml
      conf `shouldBe` Right
                        mempty { warningRules = ["DL3020", "DL3040", "SC1020"] }

    it "parse override info rules" $ do
      let yaml = [ "override:",
                   "  info:",
                   "    - DL3020",
                   "    - DL3040",
                   "    - SC1020"
                 ]
          conf = parseYaml yaml
      conf `shouldBe` Right
                        mempty { infoRules = ["DL3020", "DL3040", "SC1020"] }

    it "parse override style rules" $ do
      let yaml = [ "override:",
                   "  style:",
                   "    - DL3020",
                   "    - DL3040",
                   "    - SC1020"
                 ]
          conf = parseYaml yaml
      conf `shouldBe` Right
                        mempty { styleRules = ["DL3020", "DL3040", "SC1020"] }

    it "parse ignored rules" $ do
      let yaml = [ "ignored:",
                   "  - DL3020",
                   "  - DL3040",
                   "  - SC1020"
                 ]
          conf = parseYaml yaml
      conf `shouldBe` Right
                        mempty { ignoreRules = ["DL3020", "DL3040", "SC1020"] }

    it "parse trusted registries" $ do
      let yaml = [ "trusted-registries:",
                   "  - foobar.com",
                   "  - barfoo.com"
                 ]
          conf = parseYaml yaml
      conf `shouldBe` Right
                        mempty
                          { allowedRegistries = Set.fromList
                                                  ["foobar.com", "barfoo.com"]
                          }

    it "parse label schema" $ do
      let yaml = [ "label-schema:",
                   "  foo: email",
                   "  bar: text"
                 ]
          conf = parseYaml yaml
      conf `shouldBe` Right
                        mempty
                          { labelSchema = Map.fromList
                                            [("foo", Email), ("bar", RawText)]
                          }

    it "parse strict-labels: true" $ do
      let yaml = [ "strict-labels: true" ]
          conf = parseYaml yaml
      conf `shouldBe` Right mempty { strictLabels = Just True }

    it "parse strict-labels: false" $ do
      let yaml = [ "strict-labels: false" ]
          conf = parseYaml yaml
      conf `shouldBe` Right mempty { strictLabels = Just False }

    it "parse `failure-threshold: warning`" $ do
      let yaml = ["failure-threshold: warning"]
          conf = parseYaml yaml
      conf `shouldBe` Right mempty { failThreshold = Just DLWarningC }

    it "parse `failure-threshold: style`" $ do
      let yaml = ["failure-threshold: style"]
          conf = parseYaml yaml
      conf `shouldBe` Right mempty { failThreshold = Just DLStyleC }

-- Helper functions for parsing config files

parseYaml :: [Bytes.ByteString] -> Either String Configuration
parseYaml ls =
  case (Yaml.decode1Strict . Bytes.unlines) ls of
    Left (_, err) -> Left err
    Right res -> Right res
