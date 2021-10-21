module Hadolint.Rule.DL4001Spec (spec) where

import Data.Default
import Data.Text as Text
import Helpers
import Test.Hspec


spec :: SpecWith ()
spec = do
  let ?config = def

  describe "DL4001 - Either use Wget or Curl but not both." $ do
    it "warns when using both wget and curl" $
      let dockerFile =
            [ "FROM node as foo",
              "RUN wget my.xyz",
              "RUN curl localhost"
            ]
       in ruleCatches "DL4001" $ Text.unlines dockerFile
    it "warns when using both wget and curl in same instruction" $
      let dockerFile =
            [ "FROM node as foo",
              "RUN wget my.xyz && curl localhost"
            ]
       in ruleCatches "DL4001" $ Text.unlines dockerFile
    it "does not warn when using only wget" $
      let dockerFile =
            [ "FROM node as foo",
              "RUN wget my.xyz"
            ]
       in ruleCatchesNot "DL4001" $ Text.unlines dockerFile
    it "does not warn when using both curl and wget in different stages" $
      let dockerFile =
            [ "FROM node as foo",
              "RUN wget my.xyz",
              "FROM scratch",
              "RUN curl localhost"
            ]
       in ruleCatchesNot "DL4001" $ Text.unlines dockerFile
    it "does not warns when using both, on a single stage" $
      let dockerFile =
            [ "FROM node as foo",
              "RUN wget my.xyz",
              "RUN curl localhost",
              "FROM scratch",
              "RUN curl localhost"
            ]
       in ruleCatches "DL4001" $ Text.unlines dockerFile
    it "only warns on the relevant RUN instruction" $
      let dockerFile =
            [ "FROM node as foo",
              "RUN wget my.xyz",
              "RUN curl my.xyz",
              "RUN echo hello"
            ]
       in assertChecks
            (Text.unlines dockerFile)
            (failsWith 1 "DL4001")

    it "only warns on many relevant RUN instructions" $
      let dockerFile =
            [ "FROM node as foo",
              "RUN wget my.xyz",
              "RUN curl my.xyz",
              "RUN echo hello",
              "RUN wget foo.com"
            ]
       in assertChecks
            (Text.unlines dockerFile)
            (failsWith 2 "DL4001")
