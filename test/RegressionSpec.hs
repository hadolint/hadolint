module RegressionSpec (spec) where

import qualified Data.Text as Text
import Helpers
import Test.HUnit hiding (Label)
import Test.Hspec

spec :: SpecWith ()
spec = do
  let ?config = mempty -- default implicit parameter running the checkers
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
            [ "ARG A_WITHOUT_EQ",
              "ARG A_WITH_EQ=",
              "RUN echo bla"
            ]
       in assertChecks
            (Text.unlines dockerFile)
            (assertBool "No Warnings or Errors should be triggered" . null)
