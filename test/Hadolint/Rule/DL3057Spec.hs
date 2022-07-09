module Hadolint.Rule.DL3057Spec (spec) where

import Data.Default
import Data.Text as Text
import Helpers
import Test.Hspec


spec :: SpecWith ()
spec = do
  let ?config = def

  describe "DL3057 - `HEALTHCHECK instruction missing" $ do

    it "warn with no HEALTHCHECK instructions" $ do
      let dockerfile = Text.unlines
            [ "FROM scratch"
            ]
       in ruleCatches "DL3057" dockerfile

    it "ok when HEALTHCHECK is explicitly disabled" $ do
      let dockerfile = Text.unlines
            [ "FROM scratch",
              "HEALTHCHECK NONE"
            ]
       in ruleCatchesNot "DL3057" dockerfile

    it "ok with one HEALTHCHECK instruction" $ do
      let dockerfile = Text.unlines
            [ "FROM scratch",
              "HEALTHCHECK CMD /bin/bla"
            ]
       in ruleCatchesNot "DL3057" dockerfile

    it "ok with inheriting HEALTHCHECK instruction" $ do
      let dockerfile = Text.unlines
            [ "FROM scratch AS base",
              "HEALTHCHECK CMD /bin/bla",
              "FROM base"
            ]
       in ruleCatchesNot "DL3057" dockerfile

    it "ok with ending inheritance chain with HEALTCHECK" $ do
      let dockerfile = Text.unlines
            [ "FROM scratch AS base1",
              "FROM base1 AS base2",
              "FROM base2 AS base3",
              "FROM base3 AS end",
              "HEALTHCHECK NONE"
            ]
       in ruleCatchesNot "DL3057" dockerfile

    it "warn when inheritance chain bifurcates" $ do
      let dockerfile = Text.unlines
            [ "FROM scratch AS base1",
              "FROM base1 AS base2",
              "FROM base2 AS base3.1",
              "FROM base2 AS base3.2",  -- This will trigger DL3057
              "",
              "FROM base3.1 AS end1",
              "HEALTHCHECK NONE",
              "",
              "FROM base3.2 AS end2"  -- This will trigger DL3057
            ]
       in ruleCatches "DL3057" dockerfile

    it "warn when not inheriting with no HEALTHCHECK instruction" $ do
      let dockerfile = Text.unlines
            [ "FROM scratch AS base",
              "HEALTHCHECK CMD /bin/bla",
              "FROM scratch"
            ]
       in ruleCatches "DL3057" dockerfile
