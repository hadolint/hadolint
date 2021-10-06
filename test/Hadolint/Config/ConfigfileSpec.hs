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
      conf `shouldBe` Right
                        ( Configuration
                            (Just True)
                            Nothing
                            Nothing
                            mempty
                            mempty
                            mempty
                        )

    it "parse `no-fail: false`" $ do
      let yaml = ["no-fail: false"]
          conf = parseYaml yaml
      conf `shouldBe` Right
                        ( Configuration
                            (Just False)
                            Nothing
                            Nothing
                            mempty
                            mempty
                            mempty
                        )

    it "parse `no-color: true`" $ do
      let yaml = ["no-color: true"]
          conf = parseYaml yaml
      conf `shouldBe` Right
                        ( Configuration
                            Nothing
                            (Just True)
                            Nothing
                            mempty
                            mempty
                            mempty
                        )

    it "parse `no-color: false`" $ do
      let yaml = ["no-color: false"]
          conf = parseYaml yaml
      conf `shouldBe` Right
                        ( Configuration
                            Nothing
                            (Just False)
                            Nothing
                            mempty
                            mempty
                            mempty
                        )

    it "parse `verbose: true`" $ do
      let yaml = ["verbose: true"]
          conf = parseYaml yaml
      conf `shouldBe` Right
                        ( Configuration
                            Nothing
                            Nothing
                            (Just True)
                            mempty
                            mempty
                            mempty
                        )

    it "parse `verbose: false`" $ do
      let yaml = ["verbose: false"]
          conf = parseYaml yaml
      conf `shouldBe` Right
                        ( Configuration
                            Nothing
                            Nothing
                            (Just False)
                            mempty
                            mempty
                            mempty
                        )

    it "parse `output-format: json`" $ do
      let yaml = ["output-format: json"]
          conf = parseYaml yaml
      conf `shouldBe` Right
                        ( Configuration
                            Nothing
                            Nothing
                            Nothing
                            (Just Json)
                            mempty
                            mempty
                        )

    it "parse `output-format: sarif`" $ do
      let yaml = ["output-format: sarif"]
          conf = parseYaml yaml
      conf `shouldBe` Right
                        ( Configuration
                            Nothing
                            Nothing
                            Nothing
                            (Just Sarif)
                            mempty
                            mempty
                        )

    it "parse override error rules" $ do
      let yaml = [ "override:",
                   "  error:",
                   "    - DL3020",
                   "    - DL3040",
                   "    - SC1020"
                 ]
          conf = parseYaml yaml
      conf `shouldBe` Right
                        ( Configuration
                            Nothing
                            Nothing
                            Nothing
                            mempty
                            ( LintOptions
                                ["DL3020", "DL3040", "SC1020"]
                                mempty
                                mempty
                                mempty
                                mempty
                                mempty
                            )
                            mempty
                        )

    it "parse override warning rules" $ do
      let yaml = [ "override:",
                   "  warning:",
                   "    - DL3020",
                   "    - DL3040",
                   "    - SC1020"
                 ]
          conf = parseYaml yaml
      conf `shouldBe` Right
                        ( Configuration
                            Nothing
                            Nothing
                            Nothing
                            mempty
                            ( LintOptions
                                mempty
                                ["DL3020", "DL3040", "SC1020"]
                                mempty
                                mempty
                                mempty
                                mempty
                            )
                            mempty
                        )

    it "parse override info rules" $ do
      let yaml = [ "override:",
                   "  info:",
                   "    - DL3020",
                   "    - DL3040",
                   "    - SC1020"
                 ]
          conf = parseYaml yaml
      conf `shouldBe` Right
                        ( Configuration
                            Nothing
                            Nothing
                            Nothing
                            mempty
                            ( LintOptions
                                mempty
                                mempty
                                ["DL3020", "DL3040", "SC1020"]
                                mempty
                                mempty
                                mempty
                            )
                            mempty
                        )

    it "parse override style rules" $ do
      let yaml = [ "override:",
                   "  style:",
                   "    - DL3020",
                   "    - DL3040",
                   "    - SC1020"
                 ]
          conf = parseYaml yaml
      conf `shouldBe` Right
                        ( Configuration
                            Nothing
                            Nothing
                            Nothing
                            mempty
                            ( LintOptions
                                mempty
                                mempty
                                mempty
                                ["DL3020", "DL3040", "SC1020"]
                                mempty
                                mempty
                            )
                            mempty
                        )

    it "parse ignored rules" $ do
      let yaml = [ "ignored:",
                   "  - DL3020",
                   "  - DL3040",
                   "  - SC1020"
                 ]
          conf = parseYaml yaml
      conf `shouldBe` Right
                        ( Configuration
                            Nothing
                            Nothing
                            Nothing
                            mempty
                            ( LintOptions
                                mempty
                                mempty
                                mempty
                                mempty
                                ["DL3020", "DL3040", "SC1020"]
                                mempty
                            )
                            mempty
                        )

    it "parse trusted registries" $ do
      let yaml = [ "trusted-registries:",
                   "  - foobar.com",
                   "  - barfoo.com"
                 ]
          conf = parseYaml yaml
      conf `shouldBe` Right
                        ( Configuration
                            Nothing
                            Nothing
                            Nothing
                            mempty
                            ( LintOptions
                                mempty
                                mempty
                                mempty
                                mempty
                                mempty
                                ( RulesConfig
                                    (Set.fromList ["foobar.com", "barfoo.com"])
                                    mempty
                                    Nothing
                                )
                            )
                            mempty
                        )

    it "parse label schema" $ do
      let yaml = [ "label-schema:",
                   "  foo: email",
                   "  bar: text"
                 ]
          conf = parseYaml yaml
      conf `shouldBe` Right
                        ( Configuration
                            Nothing
                            Nothing
                            Nothing
                            mempty
                            ( LintOptions
                                mempty
                                mempty
                                mempty
                                mempty
                                mempty
                                ( RulesConfig
                                    mempty
                                    (Map.fromList [("foo", Email), ("bar", RawText)])
                                    Nothing
                                )
                            )
                            mempty
                        )

    it "parse strict-labels: true" $ do
      let yaml = [ "strict-labels: true" ]
          conf = parseYaml yaml
      conf `shouldBe` Right
                        ( Configuration
                            Nothing
                            Nothing
                            Nothing
                            mempty
                            ( LintOptions
                                mempty
                                mempty
                                mempty
                                mempty
                                mempty
                                ( RulesConfig
                                    mempty
                                    mempty
                                    (Just True)
                                )
                            )
                            mempty
                        )

    it "parse strict-labels: false" $ do
      let yaml = [ "strict-labels: false" ]
          conf = parseYaml yaml
      conf `shouldBe` Right
                        ( Configuration
                            Nothing
                            Nothing
                            Nothing
                            mempty
                            ( LintOptions
                                mempty
                                mempty
                                mempty
                                mempty
                                mempty
                                ( RulesConfig
                                    mempty
                                    mempty
                                    (Just False)
                                )
                            )
                            mempty
                        )

    it "parse `failure-threshold: warning`" $ do
      let yaml = ["failure-threshold: warning"]
          conf = parseYaml yaml
      conf `shouldBe` Right
                        ( Configuration
                            Nothing
                            Nothing
                            Nothing
                            mempty
                            mempty
                            (Just DLWarningC)
                        )

    it "parse `failure-threshold: style`" $ do
      let yaml = ["failure-threshold: style"]
          conf = parseYaml yaml
      conf `shouldBe` Right
                        ( Configuration
                            Nothing
                            Nothing
                            Nothing
                            mempty
                            mempty
                            (Just DLStyleC)
                        )

-- Helper functions for parsing config files

parseYaml :: [Bytes.ByteString] -> Either String Configuration
parseYaml ls =
  case (Yaml.decode1Strict . Bytes.unlines) ls of
    Left (_, err) -> Left err
    Right res -> Right res
