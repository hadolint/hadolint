module Hadolint.Rule.DL4004Spec (spec) where

import Data.Default
import Data.Text as Text
import Helpers
import Test.Hspec


spec :: SpecWith ()
spec = do
  let ?config = def

  describe "DL4004" $ do
    it "no cmd" $ ruleCatchesNot "DL4004" "FROM busybox"

    it "many entrypoints" $
      let dockerFile =
            [ "FROM debian",
              "ENTRYPOINT bash",
              "RUN foo",
              "ENTRYPOINT another"
            ]
       in ruleCatches "DL4004" $ Text.unlines dockerFile

    it "single entrypoint, different stages" $
      let dockerFile =
            [ "FROM debian as distro1",
              "ENTRYPOINT bash",
              "RUN foo",
              "FROM debian as distro2",
              "ENTRYPOINT another"
            ]
       in ruleCatchesNot "DL4004" $ Text.unlines dockerFile

    it "many entrypoints, different stages" $
      let dockerFile =
            [ "FROM debian as distro1",
              "ENTRYPOINT bash",
              "RUN foo",
              "ENTRYPOINT another",
              "FROM debian as distro2",
              "ENTRYPOINT another"
            ]
       in ruleCatches "DL4004" $ Text.unlines dockerFile

    it "single entry" $ ruleCatchesNot "DL4004" "ENTRYPOINT /bin/true"
    it "no entry" $ ruleCatchesNot "DL4004" "FROM busybox"
