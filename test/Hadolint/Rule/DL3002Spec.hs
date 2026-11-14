module Hadolint.Rule.DL3002Spec (spec) where

import Data.Default
import Data.Text as Text
import Helpers
import Test.Hspec


spec :: SpecWith ()
spec = do
  let ?config = def

  describe "DL3002 - Last user should not be root." $ do
    it "last user should not be root" $
      let dockerFile =
            [ "FROM scratch",
              "USER root"
            ]
       in ruleCatches "DL3002" $ Text.unlines dockerFile

    it "no root" $
      let dockerFile =
            [ "FROM scratch",
              "USER foo"
            ]
       in ruleCatchesNot "DL3002" $ Text.unlines dockerFile

    it "no root UID" $
      let dockerFile =
            [ "FROM scratch",
              "USER 0"
            ]
       in ruleCatches "DL3002" $ Text.unlines dockerFile

    it "no root:root" $
      let dockerFile =
            [ "FROM scratch",
              "USER root:root"
            ]
       in ruleCatches "DL3002" $ Text.unlines dockerFile

    it "no UID:GID" $
      let dockerFile =
            [ "FROM scratch",
              "USER 0:0"
            ]
       in ruleCatches "DL3002" $ Text.unlines dockerFile

    it "can switch back to non root" $
      let dockerFile =
            [ "FROM scratch",
              "USER root",
              "RUN something",
              "USER foo"
            ]
       in ruleCatchesNot "DL3002" $ Text.unlines dockerFile

    it "warns on transitive root user" $
      let dockerFile =
            [ "FROM debian as base",
              "USER root",
              "RUN something",
              "FROM base",
              "RUN something else"
            ]
       in ruleCatches "DL3002" $ Text.unlines dockerFile

    it "warns on multiple stages" $
      let dockerFile =
            [ "FROM debian as base",
              "USER root",
              "RUN something",
              "FROM scratch",
              "USER foo",
              "RUN something else"
            ]
       in ruleCatches "DL3002" $ Text.unlines dockerFile

    it "does not warn when switching in multiple stages" $
      let dockerFile =
            [ "FROM debian as base",
              "USER root",
              "RUN something",
              "USER foo",
              "FROM scratch",
              "RUN something else"
            ]
       in ruleCatchesNot "DL3002" $ Text.unlines dockerFile

    it "warns when an ENV value makes the last user root" $
      let dockerFile =
            [ "FROM scratch",
              "ENV user=root",
              "USER $user"
            ]
       in ruleCatches "DL3002" $ Text.unlines dockerFile

    it "warns for a braced ENV reference" $
      let dockerFile =
            [ "FROM scratch",
              "ENV user=root",
              "USER ${user}"
            ]
       in ruleCatches "DL3002" $ Text.unlines dockerFile

    it "warns when the uid comes from ENV" $
      let dockerFile =
            [ "FROM scratch",
              "ENV uid=0",
              "USER ${uid}:app"
            ]
       in ruleCatches "DL3002" $ Text.unlines dockerFile

    it "follows an ENV value that points at another variable" $
      let dockerFile =
            [ "FROM scratch",
              "ENV user=root",
              "ENV name=$user",
              "USER $name"
            ]
       in ruleCatches "DL3002" $ Text.unlines dockerFile

    it "follows a variable defined earlier in the same ENV" $
      let dockerFile =
            [ "FROM scratch",
              "ENV user=root name=$user",
              "USER $name"
            ]
       in ruleCatches "DL3002" $ Text.unlines dockerFile

    it "does not use a variable defined later in the same ENV" $
      let dockerFile =
            [ "FROM scratch",
              "ENV name=$user user=root",
              "USER $name"
            ]
       in ruleCatchesNot "DL3002" $ Text.unlines dockerFile

    it "does not warn when ENV sets a non-root user" $
      let dockerFile =
            [ "FROM scratch",
              "ENV user=app",
              "USER ${user}"
            ]
       in ruleCatchesNot "DL3002" $ Text.unlines dockerFile

    it "does not warn when the user variable was never set" $
      let dockerFile =
            [ "FROM scratch",
              "USER ${user}"
            ]
       in ruleCatchesNot "DL3002" $ Text.unlines dockerFile

    it "does not treat a default substitution as root" $
      let dockerFile =
            [ "FROM scratch",
              "USER ${user:-root}"
            ]
       in ruleCatchesNot "DL3002" $ Text.unlines dockerFile

    it "uses the latest ENV value in the same stage" $
      let dockerFile =
            [ "FROM scratch",
              "ENV user=root",
              "ENV user=app",
              "USER $user"
            ]
       in ruleCatchesNot "DL3002" $ Text.unlines dockerFile

    it "does not carry ENV into the next stage" $
      let dockerFile =
            [ "FROM scratch",
              "ENV user=root",
              "FROM scratch",
              "USER $user"
            ]
       in ruleCatchesNot "DL3002" $ Text.unlines dockerFile

    it "does not treat an escaped dollar as a variable" $
      let dockerFile =
            [ "FROM scratch",
              "ENV user=root",
              "USER $$user:0"
            ]
       in ruleCatchesNot "DL3002" $ Text.unlines dockerFile

    it "warns when a quoted user variable resolves to root" $
      let dockerFile =
            [ "FROM scratch",
              "ENV user=root",
              "USER \"$user\""
            ]
       in ruleCatches "DL3002" $ Text.unlines dockerFile
