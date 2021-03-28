module DL3025 (tests) where

import Data.Text as Text
import Helpers
import Test.Hspec


tests :: SpecWith ()
tests = do
  let ?rulesConfig = mempty
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
