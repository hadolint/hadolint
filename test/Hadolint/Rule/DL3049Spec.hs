module Hadolint.Rule.DL3049Spec (spec) where

import Data.Default
import Hadolint (Configuration (..))
import Data.Text as Text
import qualified Data.Map as Map
import qualified Hadolint.Rule as Rule
import Helpers
import Test.Hspec


spec :: SpecWith ()
spec = do
  let ?config = def { labelSchema = Map.fromList [("foo", Rule.RawText)] }

  describe "DL3049 - Missing label rule spec" $ do
  -- single stage spec
    it "not ok: single stage, no label" $ do
      ruleCatches "DL3049" "FROM baseimage"
      onBuildRuleCatches "DL3049" "FROM baseimage"
    it "not ok: single stage, wrong label" $ do
      ruleCatches "DL3049" "FROM baseimage\nLABEL bar=\"baz\""
      onBuildRuleCatches "DL3049" "FROM baseimage\nLABEL bar=\"baz\""
    it "ok: single stage, label present" $ do
      ruleCatchesNot "DL3049" "FROM baseimage\nLABEL foo=\"bar\""
      onBuildRuleCatchesNot "DL3049" "FROM baseimage\nLABEL foo=\"bar\""
  -- multi stage spec
    it "warn twice: two stages, no labels" $
      let dockerFile =
            [ "FROM stage1",
              "FROM stage2"
            ]
       in assertChecks
            (Text.unlines dockerFile)
            (failsWith 2 "DL3049")
    it "warn twice: two stages, wrong labels only" $
      let dockerFile =
            [ "FROM stage1",
              "LABEL bar=\"baz\"",
              "FROM stage2",
              "LABEL buzz=\"fuzz\""
            ]
       in assertChecks
            (Text.unlines dockerFile)
            (failsWith 2 "DL3049")
    it "warn once: two stages, label present in first only" $
      let dockerFile =
            [ "FROM baseimage",
              "LABEL foo=\"bar\"",
              "FROM newimage"
            ]
       in assertChecks
            (Text.unlines dockerFile)
            (failsWith 1 "DL3049")
    it "warn once: two stages, label present in second only" $
      let dockerFile =
            [ "FROM baseimage",
              "FROM newimage",
              "LABEL foo=\"bar\""
            ]
       in assertChecks
            (Text.unlines dockerFile)
            (failsWith 1 "DL3049")
    it "warn once: two stages, no inheritance, wrong label in one" $
      let dockerFile =
            [ "FROM baseimage",
              "LABEL baz=\"bar\"",
              "FROM newimage",
              "LABEL foo=\"bar\""
            ]
       in assertChecks
            (Text.unlines dockerFile)
            (failsWith 1 "DL3049")
    it "warn twice: two stages, inheritance, no labels" $
      let dockerFile =
            [ "FROM baseimage as base",
              "FROM base"
            ]
       in assertChecks
            (Text.unlines dockerFile)
            (failsWith 2 "DL3049")
    it "don't warn: two stages, inheritance, label defined in second stage" $
      let dockerFile =
            [ "FROM baseimage as base",
              "FROM base",
              "LABEL foo=\"bar\""
            ]
       in assertChecks
            (Text.unlines dockerFile)
            (failsWith 0 "DL3049")
    it "don't warn: two stages, no inheritance, label in last stage, with copy" $
      let dockerFile =
            [ "FROM baseimage as base",
              "FROM anotherimage",
              "LABEL foo=\"bar\"",
              "COPY --from=base /bar /baz"
            ]
       in assertChecks
            (Text.unlines dockerFile)
            (failsWith 0 "DL3049")
    it "warn once: two stages, no inheritance, label in last stage, no connection" $
      let dockerFile =
            [ "FROM baseimage as base",
              "FROM anotherimage",
              "LABEL foo=\"bar\""
            ]
       in assertChecks
            (Text.unlines dockerFile)
            (failsWith 1 "DL3049")
    it "don't warn: two stages, inheritance" $
      let dockerFile =
            [ "FROM baseimage as base",
              "LABEL foo=\"bar\"",
              "FROM base"
            ]
       in assertChecks
            (Text.unlines dockerFile)
            (failsWith 0 "DL3049")
    it "warn once: multiple results, no inheritance, label in one result" $
      let dockerFile =
            [ "FROM baseimage as base",
              "FROM newimage",
              "LABEL foo=\"bar\"",
              "COPY --from=base /bar /baz",
              "",
              "FROM newimage",
              "COPY --from=base /bar /baz"
            ]
       in assertChecks
            (Text.unlines dockerFile)
            (failsWith 1 "DL3049")
    it "warn once: multiple results, inheritance, label in one result" $
      let dockerFile =
            [ "FROM baseimage as base",
              "FROM base",
              "LABEL foo=\"bar\"",
              "",
              "FROM base"
            ]
       in assertChecks
            (Text.unlines dockerFile)
            (failsWith 1 "DL3049")
    it "warn once: multiple results, inheritance, label in one result" $
      let dockerFile =
            [ "FROM baseimage as base",
              "FROM base",
              "",
              "FROM base",
              "LABEL foo=\"bar\""
            ]
       in assertChecks
            (Text.unlines dockerFile)
            (failsWith 1 "DL3049")
