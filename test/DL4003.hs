module DL4003 (tests) where

import Data.Text as Text
import Helpers
import Test.Hspec


tests :: SpecWith ()
tests = do
  let ?rulesConfig = mempty
  describe "DL4003" $ do
    it "many cmds" $
      let dockerFile =
            [ "FROM debian",
              "CMD bash",
              "RUN foo",
              "CMD another"
            ]
       in ruleCatches "DL4003" $ Text.unlines dockerFile

    it "single cmds, different stages" $
      let dockerFile =
            [ "FROM debian as distro1",
              "CMD bash",
              "RUN foo",
              "FROM debian as distro2",
              "CMD another"
            ]
       in ruleCatchesNot "DL4003" $ Text.unlines dockerFile

    it "many cmds, different stages" $
      let dockerFile =
            [ "FROM debian as distro1",
              "CMD bash",
              "RUN foo",
              "CMD another",
              "FROM debian as distro2",
              "CMD another"
            ]
       in ruleCatches "DL4003" $ Text.unlines dockerFile

    it "single cmd" $ ruleCatchesNot "DL4003" "CMD /bin/true"
