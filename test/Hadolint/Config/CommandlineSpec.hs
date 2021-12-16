module Hadolint.Config.CommandlineSpec (spec) where

import Test.Hspec
import Test.HUnit hiding (Label)
import Hadolint
import Options.Applicative
  ( ParserResult (..),
    defaultPrefs,
    execParserPure,
    fullDesc,
    info,
  )
import qualified Data.Map as Map
import qualified Data.Set as Set


spec :: SpecWith ()
spec = do
  describe "Config from Commandline" $ do
    it "parse with no arguments given" $ do
      checkCommandline [] $ CommandlineConfig
                              False
                              Nothing
                              []
                              Nothing
                              mempty

    describe "parse version flag" $ do
      it "parse -v" $ do
        checkCommandline ["-v"] $ CommandlineConfig
                                True
                                Nothing
                                []
                                Nothing
                                mempty

      it "parse --version" $ do
        checkCommandline ["--version"] $ CommandlineConfig
                                True
                                Nothing
                                []
                                Nothing
                                mempty

    describe "parse config file option" $ do
      it "parse -c hadolint.yaml" $ do
        checkCommandline ["-c", "hadolint.yaml"] $ CommandlineConfig
                                False
                                (Just "hadolint.yaml")
                                []
                                Nothing
                                mempty

      it "parse --config hadolint.yaml" $ do
        checkCommandline ["--config", "hadolint.yaml"] $ CommandlineConfig
                                False
                                (Just "hadolint.yaml")
                                []
                                Nothing
                                mempty

    describe "parse file arguments" $ do
      it "parse `Dockerfile`" $ do
        checkCommandline ["Dockerfile"] $ CommandlineConfig
                                False
                                Nothing
                                ["Dockerfile"]
                                Nothing
                                mempty

      it "parse `Dockerfile1 Dockerfile2`" $ do
        checkCommandline ["Dockerfile1", "Dockerfile2"] $ CommandlineConfig
                                False
                                Nothing
                                ["Dockerfile1", "Dockerfile2"]
                                Nothing
                                mempty

      it "parse --file-path-in-report foobar/Dockerfile" $ do
        checkCommandline
          ["--file-path-in-report", "foobar/Dockerfile"]
          ( CommandlineConfig
              False
              Nothing
              []
              (Just "foobar/Dockerfile")
              mempty
          )

    describe "parse general configuration" $ do
      it "parse --no-fail" $ do
        checkCommandline ["--no-fail"] $ CommandlineConfig
                                          False
                                          Nothing
                                          []
                                          Nothing
                                          mempty { partialNoFail = Just True }

      it "parse --no-color" $ do
        checkCommandline ["--no-color"] $ CommandlineConfig
                                            False
                                            Nothing
                                            []
                                            Nothing
                                            mempty { partialNoColor = Just True }

      it "parse -V" $ do
        checkCommandline ["-V"] $ CommandlineConfig
                                False
                                Nothing
                                []
                                Nothing
                                mempty { partialVerbose = Just True }

      it "parse --verbose" $ do
        checkCommandline ["--verbose"] $ CommandlineConfig
                                False
                                Nothing
                                []
                                Nothing
                                mempty { partialVerbose = Just True }

      it "parse -f json" $ do
        checkCommandline ["-f", "json"] $ CommandlineConfig
                                False
                                Nothing
                                []
                                Nothing
                                mempty { partialFormat = Just Json }

      it "parse --format" $ do
        checkCommandline ["--format", "sarif"] $ CommandlineConfig
                                False
                                Nothing
                                []
                                Nothing
                                mempty { partialFormat = Just Sarif }

    describe "parse severity overrides" $ do
      it "parse --error=DL3010" $ do
        checkCommandline ["--error", "DL3010"] $ CommandlineConfig
                                False
                                Nothing
                                []
                                Nothing
                                mempty { partialErrorRules = ["DL3010"] }

      it "parse --error=DL3010 --error=DL3020" $ do
        checkCommandline
          ["--error", "DL3010", "--error=DL3020"]
          ( CommandlineConfig
              False
              Nothing
              []
              Nothing
              mempty { partialErrorRules = ["DL3010", "DL3020"] }
          )

      it "parse --warning=DL3010" $ do
        checkCommandline ["--warning", "DL3010"] $ CommandlineConfig
                                False
                                Nothing
                                []
                                Nothing
                                mempty { partialWarningRules = ["DL3010"] }

      it "parse --warning=DL3010 --warning=DL3020" $ do
        checkCommandline
          ["--warning", "DL3010", "--warning=DL3020"]
          ( CommandlineConfig
              False
              Nothing
              []
              Nothing
              mempty { partialWarningRules = ["DL3010", "DL3020"] }
          )

      it "parse --info=DL3010" $ do
        checkCommandline ["--info", "DL3010"] $ CommandlineConfig
                                False
                                Nothing
                                []
                                Nothing
                                mempty { partialInfoRules = ["DL3010"] }

      it "parse --info=DL3010 --info=DL3020" $ do
        checkCommandline
          ["--info", "DL3010", "--info=DL3020"]
          ( CommandlineConfig
              False
              Nothing
              []
              Nothing
              mempty { partialInfoRules = ["DL3010", "DL3020"] }
          )

      it "parse --style=DL3010" $ do
        checkCommandline ["--style", "DL3010"] $ CommandlineConfig
                                False
                                Nothing
                                []
                                Nothing
                                mempty { partialStyleRules = ["DL3010"] }

      it "parse --style=DL3010 --style=DL3020" $ do
        checkCommandline
          ["--style", "DL3010", "--style=DL3020"]
          ( CommandlineConfig
              False
              Nothing
              []
              Nothing
              mempty { partialStyleRules = ["DL3010", "DL3020"] }
          )

      it "parse --ignore=DL3010" $ do
        checkCommandline ["--ignore", "DL3010"] $ CommandlineConfig
                                False
                                Nothing
                                []
                                Nothing
                                mempty { partialIgnoreRules = ["DL3010"] }

      it "parse --ignore=DL3010 --ignore=DL3020" $ do
        checkCommandline
          ["--ignore", "DL3010", "--ignore=DL3020"]
          ( CommandlineConfig
              False
              Nothing
              []
              Nothing
              mempty { partialIgnoreRules = ["DL3010", "DL3020"] }
          )

    describe "parse trusted registries" $ do
      it "parse --trusted-registry foobar.com" $ do
        checkCommandline
          ["--trusted-registry", "foobar.com"]
          ( CommandlineConfig
              False
              Nothing
              []
              Nothing
              mempty { partialAllowedRegistries = Set.fromList ["foobar.com"] }
          )

      it "parse --trusted-registry foobar.com --trusted-registry barfoo.io" $ do
        checkCommandline
          [ "--trusted-registry",
            "foobar.com",
            "--trusted-registry",
            "barfoo.io"
          ]
          ( CommandlineConfig
              False
              Nothing
              []
              Nothing
              mempty
                { partialAllowedRegistries =
                    Set.fromList ["foobar.com", "barfoo.io"]
                }
          )

    describe "parse required labels" $ do
      it "parse --require-label foo:email" $ do
        checkCommandline
          ["--require-label", "foo:email"]
          ( CommandlineConfig
              False
              Nothing
              []
              Nothing
              mempty { partialLabelSchema = Map.fromList [("foo", Email)] }
          )

      it "parse --require-label foo:email --require-label bar:text" $ do
        checkCommandline
          ["--require-label", "foo:email", "--require-label", "bar:text"]
          ( CommandlineConfig
              False
              Nothing
              []
              Nothing
              mempty
                { partialLabelSchema =
                    Map.fromList [("foo", Email), ("bar", RawText)]
                }
          )

    describe "parse strict labels" $ do
      it "parse --strict-labels" $ do
        checkCommandline
          ["--strict-labels"]
          ( CommandlineConfig
              False
              Nothing
              []
              Nothing
              mempty { partialStrictLabels = Just True }
          )

    describe "parse disable ignore pragma" $ do
      it "parse --disable-ignore-pragma" $ do
        checkCommandline
          ["--disable-ignore-pragma"]
          ( CommandlineConfig
              False
              Nothing
              []
              Nothing
              mempty { partialDisableIgnorePragma = Just True }
          )

    describe "parse failure thresholds" $ do
      it "parse -t warning" $ do
        checkCommandline ["-t", "warning"] $ CommandlineConfig
                                False
                                Nothing
                                []
                                Nothing
                                mempty
                                  { partialFailureThreshold = Just DLWarningC }

      it "parse --failure-threshold style" $ do
        checkCommandline ["--failure-threshold", "style"] $ CommandlineConfig
                                False
                                Nothing
                                []
                                Nothing
                                mempty
                                  { partialFailureThreshold = Just DLStyleC }

    describe "fail parsing on garbage input" $ do
      it "fail parsing --blafoo" $ do
        checkCommandlineFail
          ["--blafoo"]

      it "fail parsing --require-label foo:bar" $ do
        checkCommandlineFail
          ["--require-label", "foo:bar"]


checkCommandline :: [String] -> CommandlineConfig -> Assertion
checkCommandline args config = do
  let inf = info parseCommandline fullDesc
      res = execParserPure defaultPrefs inf args
  case res of
    Success cfg -> cfg `shouldBe` config
    _ -> assertFailure "should have parsed commandline arguments"

checkCommandlineFail :: [String] -> Assertion
checkCommandlineFail args = do
  let inf = info parseCommandline fullDesc
      res = execParserPure defaultPrefs inf args
  case res of
    Failure _ -> assertEqual "" (0 :: Int) (0 :: Int)  -- inverse asserFailure
    _ -> assertFailure "should have failed parsing commandline arguments"
