module RegressionSpec (spec) where

import Data.Default
import qualified Data.Text as Text
import Helpers
import Test.HUnit hiding (Label)
import Test.Hspec


spec :: SpecWith ()
spec = do
  let ?config = def  -- default implicit parameter running the checkers

  describe "Regression Tests" $ do
    it "Comments with backslashes at the end are just comments" $
      let dockerFile =
            [ "FROM alpine:3.6",
              "# The following comment makes hadolint still complain about DL4006",
              "# \\",
              "# should solve DL4006",
              "SHELL [\"/bin/sh\", \"-o\", \"pipefail\", \"-c\"]",
              "# RUN with pipe. causes DL4006, but should be fixed by above SHELL",
              "RUN echo \"kaka\" | sed 's/a/o/g' >> /root/afile"
            ]
       in ruleCatches "DL4006" $ Text.unlines dockerFile

    it "`ARG` can correctly unset variables" $
      let dockerFile =
            Text.unlines
              [ "FROM alpine:3",  -- to satisfy DL3061
                "ARG A_WITHOUT_EQ",
                "ARG A_WITH_EQ=",
                "HEALTHCHECK NONE",  -- to satisfy DL3057, even though it is ignored by default
                "RUN echo bla"
              ]
       in assertChecks
            dockerFile
            (assertBool "No Warnings or Errors should be triggered," . null)
