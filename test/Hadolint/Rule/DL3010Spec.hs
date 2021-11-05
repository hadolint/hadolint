module Hadolint.Rule.DL3010Spec (spec) where

import Data.Default
import Data.Text as Text
import Helpers
import Test.Hspec


spec :: SpecWith ()
spec = do
  let ?config = def

  describe "DL3010 - Use `ADD` for extracting archives into an image" $ do
    it "catch: copy archive then extract 1" $
      let dockerFile =
            [ "COPY packaged-app.tar /usr/src/app",
              "RUN tar -xf /usr/src/app/packaged-app.tar"
            ]
      in do
        ruleCatches "DL3010" $ Text.unlines dockerFile
    it "catch: copy archive then extract 2" $
      let dockerFile =
            [ "COPY packaged-app.tar /usr/src/app",
              "WORKDIR /usr/src/app",
              "RUN foo bar && echo something && tar -xf packaged-app.tar"
            ]
      in do
        ruleCatches "DL3010" $ Text.unlines dockerFile
    it "catch: copy archive then extract 3" $
      let dockerFile =
            [ "COPY foo/bar/packaged-app.tar /foo.tar",
              "RUN tar -xf /foo.tar"
            ]
      in do
        ruleCatches "DL3010" $ Text.unlines dockerFile
    it "catch: copy archive then extract windows paths 1" $
      let dockerFile =
            [ "COPY build\\foo\\bar.tar.gz \"C:\\Program Files\\Foo\"",
              "RUN tar -xf \"C:\\Program Files\\Foo\\bar.tar.gz\""
            ]
      in do
        ruleCatches "DL3010" $ Text.unlines dockerFile
    it "catch: copy archive then extract windows paths 2" $
      let dockerFile =
            [ "COPY build\\foo\\bar.tar.gz \"C:\\Program Files\\foo.tar.gz\"",
              "RUN tar -xf foo.tar.gz"
            ]
      in do
        ruleCatches "DL3010" $ Text.unlines dockerFile
    it "ignore: copy archive without extract" $
      let dockerFile =
            [ "COPY packaged-app.tar /usr/src/app",
              "FROM debian:11 as newstage"
            ]
      in do
        ruleCatchesNot "DL3010" $ Text.unlines dockerFile
    it "ignore: non archive" $
      ruleCatchesNot "DL3010" "COPY package.json /usr/src/app"
    it "ignore: copy from previous stage" $
      ruleCatchesNot "DL3010"
        "COPY --from=builder /usr/local/share/some.tar /opt/some.tar"
