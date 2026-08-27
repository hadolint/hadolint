module Hadolint.Rule.DL3009Spec (spec) where

import Data.Default
import Data.Text as Text
import Helpers
import Test.Hspec


spec :: SpecWith ()
spec = do
  let ?config = def

  describe "DL3009 - Use BuildKit cache mounts for apt." $ do
    it "warn: apt-get update without cache mount" $
      let dockerFile =
            [ "FROM scratch",
              "RUN apt-get update && apt-get install python"
            ]
       in do
            ruleCatches "DL3009" $ Text.unlines dockerFile
            onBuildRuleCatches "DL3009" $ Text.unlines dockerFile

    it "warn: rm -rf lists no longer suppresses" $
      let dockerFile =
            [ "FROM scratch",
              "RUN apt-get update && apt-get install python && rm -rf /var/lib/apt/lists/*"
            ]
       in do
            ruleCatches "DL3009" $ Text.unlines dockerFile
            onBuildRuleCatches "DL3009" $ Text.unlines dockerFile

    it "warn: intermediate stage with rm -rf lists no longer suppresses" $
      let dockerFile =
            [ "FROM ubuntu as foo",
              "RUN apt-get update && apt-get install python && rm -rf /var/lib/apt/lists/*",
              "FROM foo",
              "RUN hey!"
            ]
       in do
            ruleCatches "DL3009" $ Text.unlines dockerFile
            onBuildRuleCatches "DL3009" $ Text.unlines dockerFile

    it "warn: apt update in last stage without cache mount" $
      let dockerFile =
            [ "FROM ubuntu as foo",
              "RUN hey!",
              "FROM scratch",
              "RUN apt-get update && apt-get install python"
            ]
       in do
            ruleCatches "DL3009" $ Text.unlines dockerFile
            onBuildRuleCatches "DL3009" $ Text.unlines dockerFile

    it "warn: apt update in any stage without cache mount" $
      let dockerFile =
            [ "FROM ubuntu as foo",
              "RUN apt-get update && apt-get install python",
              "FROM foo",
              "RUN hey!"
            ]
       in do
            ruleCatches "DL3009" $ Text.unlines dockerFile
            onBuildRuleCatches "DL3009" $ Text.unlines dockerFile

    it "warn: apt update without cache mount regardless of stage reuse" $
      let dockerFile =
            [ "FROM ubuntu as foo",
              "RUN apt-get update && apt-get install python",
              "FROM scratch",
              "RUN hey!"
            ]
       in do
            ruleCatches "DL3009" $ Text.unlines dockerFile
            onBuildRuleCatches "DL3009" $ Text.unlines dockerFile

    it "warn: apt update without cache mount" $
      let dockerFile =
            [ "FROM scratch",
              "RUN apt update && apt install python"
            ]
       in do
            ruleCatches "DL3009" $ Text.unlines dockerFile
            onBuildRuleCatches "DL3009" $ Text.unlines dockerFile

    it "warn: apt update even with rm -rf" $
      let dockerFile =
            [ "FROM scratch",
              "RUN apt update && apt install python && rm -rf /var/lib/apt/lists/*"
            ]
       in do
            ruleCatches "DL3009" $ Text.unlines dockerFile
            onBuildRuleCatches "DL3009" $ Text.unlines dockerFile

    it "warn: aptitude update without cache mount" $
      let dockerFile =
            [ "FROM scratch",
              "RUN aptitude update && aptitude install python"
            ]
       in do
            ruleCatches "DL3009" $ Text.unlines dockerFile
            onBuildRuleCatches "DL3009" $ Text.unlines dockerFile

    it "warn: aptitude update even with rm -rf" $
      let dockerFile =
            [ "FROM scratch",
              "RUN aptitude update && aptitude install python && rm -rf /var/lib/apt/lists/*"
            ]
       in do
            ruleCatches "DL3009" $ Text.unlines dockerFile
            onBuildRuleCatches "DL3009" $ Text.unlines dockerFile

    it "don't warn: BuildKit cache mount to apt lists directory" $ do
      ruleCatchesNot
        "DL3009"
        "RUN --mount=type=cache,target=/var/lib/apt/lists \\\
        \    apt-get update && apt-get install python"
      onBuildRuleCatchesNot
        "DL3009"
        "RUN --mount=type=cache,target=/var/lib/apt/lists \\\
        \    apt-get update && apt-get install python"

    it "don't warn: BuildKit cache mount to both apt directories" $ do
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

    it "don't warn: BuildKit cache mount to both apt directories (multiline)" $ do
      let dockerFile =
            [ "RUN rm -f /etc/apt/apt.conf.d/docker-clean",
              "RUN --mount=type=cache,target=/var/cache/apt \\",
              "    --mount=type=cache,target=/var/lib/apt \\",
              "    apt-get update && apt-get install foo"
            ]
      ruleCatchesNot "DL3009" $ Text.unlines dockerFile
      onBuildRuleCatchesNot "DL3009" $ Text.unlines dockerFile

    it "warn: cache mount to apt cache directory only" $ do
      let dockerFile =
            [ "RUN --mount=type=cache,target=/var/cache/apt \\",
              "    rm -f /etc/apt/apt.conf.d/docker-clean && \\",
              "    apt-get update && apt-get install python"
            ]
       in do
            ruleCatches "DL3009" $ Text.unlines dockerFile
            onBuildRuleCatches "DL3009" $ Text.unlines dockerFile

    it "warn: cache mount to apt lists directory only" $ do
      let dockerFile =
            [ "RUN rm -f /etc/apt/apt.conf.d/docker-clean",
              "RUN --mount=type=cache,target=/var/lib/apt \\",
              "    apt-get update && apt-get install python"
            ]
       in do
            ruleCatches "DL3009" $ Text.unlines dockerFile
            onBuildRuleCatches "DL3009" $ Text.unlines dockerFile

    it "don't warn: tmpfs mount to apt cache and lists directory" $
      let dockerFile =
            [ "RUN \\",
              "  --mount=type=tmpfs,target=/var/cache/apt \\",
              "  --mount=type=tmpfs,target=/var/lib/apt \\",
              "  apt-get update"
            ]
       in do
            ruleCatchesNot "DL3009" $ Text.unlines dockerFile
            onBuildRuleCatchesNot "DL3009" $ Text.unlines dockerFile

    it "don't warn: tmpfs + cache mix on apt directories" $
      let dockerFile =
            [ "RUN \\",
              "  --mount=type=tmpfs,target=/var/cache/apt \\",
              "  --mount=type=cache,target=/var/lib/apt \\",
              "  apt-get update"
            ]
       in do
            ruleCatchesNot "DL3009" $ Text.unlines dockerFile
            onBuildRuleCatchesNot "DL3009" $ Text.unlines dockerFile

    it "don't warn: cache + tmpfs mix on apt directories" $
      let dockerFile =
            [ "RUN \\",
              "  --mount=type=cache,target=/var/cache/apt \\",
              "  --mount=type=tmpfs,target=/var/lib/apt \\",
              "  apt-get update"
            ]
       in do
            ruleCatchesNot "DL3009" $ Text.unlines dockerFile
            onBuildRuleCatchesNot "DL3009" $ Text.unlines dockerFile

    it "don't warn: cache mounts on both apt directories" $
      let dockerFile =
            [ "RUN \\",
              "  --mount=type=cache,target=/var/cache/apt \\",
              "  --mount=type=cache,target=/var/lib/apt \\",
              "  apt-get update"
            ]
       in do
            ruleCatchesNot "DL3009" $ Text.unlines dockerFile
            onBuildRuleCatchesNot "DL3009" $ Text.unlines dockerFile
