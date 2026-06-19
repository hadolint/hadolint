module Hadolint.Rule.DL3025Spec (spec) where

import Data.Default
import Data.Text as Text
import Helpers
import Test.Hspec


spec :: SpecWith ()
spec = do
  let ?config = def

  describe "DL3025 - Use arguments JSON notation for `CMD` and `ENTRYPOINT` arguments" $ do

    it "warn on ENTRYPOINT" $
      let dockerFile =
            [ "FROM node as foo",
              "ENTRYPOINT something"
            ]
       in ruleCatches "DL3025" $ Text.unlines dockerFile

    it "don't warn on ENTRYPOINT json notation" $
      let dockerFile =
            [ "FROM scratch as build",
              "ENTRYPOINT [\"foo\", \"bar\"]"
            ]
       in ruleCatchesNot "DL3025" $ Text.unlines dockerFile

    it "warn on CMD" $
      let dockerFile =
            [ "FROM node as foo",
              "CMD something"
            ]
       in ruleCatches "DL3025" $ Text.unlines dockerFile

    it "don't warn on CMD json notation" $
      let dockerFile =
            [ "FROM scratch as build",
              "CMD [\"foo\", \"bar\"]",
              "CMD [ \"foo\", \"bar\" ]"
            ]
       in ruleCatchesNot "DL3025" $ Text.unlines dockerFile

    it "warn on HEALTHCHECK CMD shell notation" $
      let df = Text.unlines [ "HEALTHCHECK CMD foobar" ]
       in ruleCatches "DL3025" df

    it "don't warn on HEALTHCHECK CMD json notation" $
      let df = Text.unlines [ "HEALTHCHECK CMD [ \"foobar\" ]" ]
       in ruleCatchesNot "DL3025" df

    -- regression: deal with broken long strings in exec format
    it "don't warn on CMD JSON notation with broken long strings" $
      let dockerFile =
            [ "CMD [ \"/bin/sh\", \"-c\", \\",
              "      \"echo foo && \\",
              "       echo bar\" \\",
              "    ]"
            ]
       in ruleCatchesNot "DL3025" $ Text.unlines dockerFile
