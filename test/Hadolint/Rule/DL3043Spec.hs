module Hadolint.Rule.DL3043Spec (spec) where

import Data.Default
import Data.Text as Text
import Helpers
import Test.Hspec


spec :: SpecWith ()
spec = do
  let ?config = def

  describe "DL3043 - `ONBUILD`, `FROM` or `MAINTAINER` triggered from within `ONBUILD` instruction." $ do
    it "error when using `ONBUILD` within `ONBUILD`" $
      let dockerFile =
            [ "ONBUILD ONBUILD RUN anything"
            ]
       in ruleCatches "DL3043" $ Text.unlines dockerFile
    it "error when using `FROM` within `ONBUILD`" $
      let dockerFile =
            [ "ONBUILD FROM debian:buster"
            ]
       in ruleCatches "DL3043" $ Text.unlines dockerFile
    it "error when using `MAINTAINER` within `ONBUILD`" $
      let dockerFile =
            [ "ONBUILD MAINTAINER \"BoJack Horseman\""
            ]
       in ruleCatches "DL3043" $ Text.unlines dockerFile
    it "ok with `ADD`" $ ruleCatchesNot "DL3043" "ONBUILD ADD anything anywhere"
    it "ok with `USER`" $ ruleCatchesNot "DL3043" "ONBUILD USER anything"
    it "ok with `LABEL`" $ ruleCatchesNot "DL3043" "ONBUILD LABEL bla=\"blubb\""
    it "ok with `STOPSIGNAL`" $ ruleCatchesNot "DL3043" "ONBUILD STOPSIGNAL anything"
    it "ok with `COPY`" $ ruleCatchesNot "DL3043" "ONBUILD COPY anything anywhere"
    it "ok with `RUN`" $ ruleCatchesNot "DL3043" "ONBUILD RUN anything"
    it "ok with `CMD`" $ ruleCatchesNot "DL3043" "ONBUILD CMD anything"
    it "ok with `SHELL`" $ ruleCatchesNot "DL3043" "ONBUILD SHELL anything"
    it "ok with `WORKDIR`" $ ruleCatchesNot "DL3043" "ONBUILD WORKDIR anything"
    it "ok with `EXPOSE`" $ ruleCatchesNot "DL3043" "ONBUILD EXPOSE 69"
    it "ok with `VOLUME`" $ ruleCatchesNot "DL3043" "ONBUILD VOLUME anything"
    it "ok with `ENTRYPOINT`" $ ruleCatchesNot "DL3043" "ONBUILD ENTRYPOINT anything"
    it "ok with `ENV`" $ ruleCatchesNot "DL3043" "ONBUILD ENV MYVAR=\"bla\""
    it "ok with `ARG`" $ ruleCatchesNot "DL3043" "ONBUILD ARG anything"
    it "ok with `HEALTHCHECK`" $ ruleCatchesNot "DL3043" "ONBUILD HEALTHCHECK NONE"
    it "ok with `FROM` outside of `ONBUILD`" $ ruleCatchesNot "DL3043" "FROM debian:buster"
    it "ok with `MAINTAINER` outside of `ONBUILD`" $ ruleCatchesNot "DL3043" "MAINTAINER \"Some Guy\""
