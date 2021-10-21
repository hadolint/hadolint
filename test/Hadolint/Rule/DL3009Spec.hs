module Hadolint.Rule.DL3009Spec (spec) where

import Data.Default
import Data.Text as Text
import Helpers
import Test.Hspec


spec :: SpecWith ()
spec = do
  let ?config = def

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
      ruleCatchesNot
        "DL3009"
        "RUN --mount=type=cache,target=/var/lib/apt/lists \\\
        \    apt-get update && apt-get install python"
      onBuildRuleCatchesNot
        "DL3009"
        "RUN --mount=type=cache,target=/var/lib/apt/lists \\\
        \    apt-get update && apt-get install python"

    it "don't warn: BuildKit cache mount to apt directories 1" $ do
      ruleCatchesNot
        "DL3009"
        "RUN --mount=type=cache,target=/var/lib/apt \\\
        \    --mount=type=cache,target=/var/cache/apt \\\
        \    rm -f /etc/apt/apt.conf.d/docker-clean && \\\
        \    apt-get update && apt-get install python"
      onBuildRuleCatchesNot
        "DL3009"
        "RUN --mount=type=cache,target=/var/lib/apt \\\
        \    --mount=type=cache,target=/var/cache/apt \\\
        \    rm -f /etc/apt/apt.conf.d/docker-clean && \\\
        \    apt-get update && apt-get install python"

    it "don't warn: BuildKit cache mount to apt directories 2" $ do
      let dockerFile =
            [ "RUN rm -f /etc/apt/apt.conf.d/docker-clean",
              "RUN --mount=type=cache,target=/var/cache/apt \\",
              "    --mount=type=cache,target=/var/lib/apt \\",
              "    apt-get update && apt-get install foo"
            ]
      ruleCatchesNot "DL3009" $ Text.unlines dockerFile
      onBuildRuleCatchesNot "DL3009" $ Text.unlines dockerFile

    it "warn: BuildKit cache mount to apt cache directory only" $ do
      let dockerFile =
            [ "RUN --mount=type=cache,target=/var/cache/apt \\",
              "    rm -f /etc/apt/apt.conf.d/docker-clean && \\",
              "    apt-get update && apt-get install python"
            ]
       in do
            ruleCatches "DL3009" $ Text.unlines dockerFile
            onBuildRuleCatches "DL3009" $ Text.unlines dockerFile

    it "warn: BuildKit cache mount to apt lists directory only" $ do
      let dockerFile =
            [ "RUN rm -f /etc/apt/apt.conf.d/docker-clean",
              "RUN --mount=type=cache,target=/var/lib/apt \\",
              "    apt-get update && apt-get install python"
            ]
       in do
            ruleCatches "DL3009" $ Text.unlines dockerFile
            onBuildRuleCatches "DL3009" $ Text.unlines dockerFile
