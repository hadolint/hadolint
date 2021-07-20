module DL3009 (tests) where

import Data.Text as Text
import Helpers
import Test.Hspec


tests :: SpecWith ()
tests = do
  let ?rulesConfig = mempty
  describe "DL3009 - Delete the apt-get lists after installing something." $ do
    it "apt-get no cleanup" $
      let dockerFile =
            [ "FROM scratch",
              "RUN apt-get update && apt-get install python"
            ]
       in do
            ruleCatches "DL3009" $ Text.unlines dockerFile
            onBuildRuleCatches "DL3009" $ Text.unlines dockerFile
    it "apt-get cleanup in stage image" $
      let dockerFile =
            [ "FROM ubuntu as foo",
              "RUN apt-get update && apt-get install python",
              "FROM scratch",
              "RUN echo hey!"
            ]
       in do
            ruleCatchesNot "DL3009" $ Text.unlines dockerFile
            onBuildRuleCatches "DL3009" $ Text.unlines dockerFile
    it "apt-get no cleanup in last stage" $
      let dockerFile =
            [ "FROM ubuntu as foo",
              "RUN hey!",
              "FROM scratch",
              "RUN apt-get update && apt-get install python"
            ]
       in do
            ruleCatches "DL3009" $ Text.unlines dockerFile
            onBuildRuleCatches "DL3009" $ Text.unlines dockerFile
    it "apt-get no cleanup in intermediate stage" $
      let dockerFile =
            [ "FROM ubuntu as foo",
              "RUN apt-get update && apt-get install python",
              "FROM foo",
              "RUN hey!"
            ]
       in do
            ruleCatches "DL3009" $ Text.unlines dockerFile
            onBuildRuleCatches "DL3009" $ Text.unlines dockerFile
    it "no warn apt-get cleanup in intermediate stage that cleans lists" $
      let dockerFile =
            [ "FROM ubuntu as foo",
              "RUN apt-get update && apt-get install python && rm -rf /var/lib/apt/lists/*",
              "FROM foo",
              "RUN hey!"
            ]
       in do
            ruleCatchesNot "DL3009" $ Text.unlines dockerFile
            onBuildRuleCatchesNot "DL3009" $ Text.unlines dockerFile
    it "no warn apt-get cleanup in intermediate stage when stage not used later" $
      let dockerFile =
            [ "FROM ubuntu as foo",
              "RUN apt-get update && apt-get install python",
              "FROM scratch",
              "RUN hey!"
            ]
       in do
            ruleCatchesNot "DL3009" $ Text.unlines dockerFile
            onBuildRuleCatches "DL3009" $ Text.unlines dockerFile
    it "apt-get cleanup" $
      let dockerFile =
            [ "FROM scratch",
              "RUN apt-get update && apt-get install python && rm -rf /var/lib/apt/lists/*"
            ]
       in do
            ruleCatchesNot "DL3009" $ Text.unlines dockerFile
            onBuildRuleCatchesNot "DL3009" $ Text.unlines dockerFile

    it "don't warn: BuildKit cache mount to apt lists directory" $ do
      ruleCatchesNot "DL3009" "RUN --mount=type=cache,target=/var/lib/apt/lists apt-get update && apt-get install python"
      onBuildRuleCatchesNot "DL3009" "RUN --mount=type=cache,target=/var/lib/apt/lists apt-get update && apt-get install python"
