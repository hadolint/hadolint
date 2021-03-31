import qualified ConfigSpec
import qualified Data.Text as Text
import qualified DL3000
import qualified DL3001
import qualified DL3002
import qualified DL3003
import qualified DL3004
import qualified DL3005
import qualified DL3006
import qualified DL3007
import qualified DL3008
import qualified DL3009
import qualified DL3010
import qualified DL3011
import qualified DL3012
import qualified DL3013
import qualified DL3014
import qualified DL3015
import qualified DL3016
import qualified DL3017
import qualified DL3018
import qualified DL3019
import qualified DL3020
import qualified DL3021
import qualified DL3022
import qualified DL3023
import qualified DL3024
import qualified DL3025
import qualified DL3026
import qualified DL3027
import qualified DL3028
import qualified DL3029
import qualified DL3030
import qualified DL3031
import qualified DL3032
import qualified DL3033
import qualified DL3034
import qualified DL3035
import qualified DL3036
import qualified DL3037
import qualified DL3038
import qualified DL3039
import qualified DL3040
import qualified DL3041
import qualified DL3042
import qualified DL3043
import qualified DL3044
import qualified DL3045
import qualified DL3046
import qualified DL3047
import qualified DL3048
import qualified DL3049
import qualified DL3050
import qualified DL3051
import qualified DL3052
import qualified DL3053
import qualified DL3054
import qualified DL3055
import qualified DL3056
import qualified DL3057
import qualified DL3058
import qualified DL4001
import qualified DL4003
import qualified DL4004
import qualified DL4005
import qualified DL4006
import Hadolint.Formatter.TTY (formatError)
import Helpers
import Language.Docker.Parser
import qualified Shellcheck
import qualified ShellSpec
import Test.HUnit hiding (Label)
import Test.Hspec

main :: IO ()
main =
  hspec $ do
    let ?rulesConfig = mempty -- default implicit parameter running the checkers
    describe "format error" $
      it "display error after line pos" $ do
        let ast = parseText "FOM debian:jessie"
            expectedMsg =
              "<string>:1:1 unexpected 'F' expecting '#', ADD, ARG, CMD, COPY, ENTRYPOINT, "
                <> "ENV, EXPOSE, FROM, HEALTHCHECK, LABEL, MAINTAINER, ONBUILD, RUN, SHELL, STOPSIGNAL, "
                <> "USER, VOLUME, WORKDIR, or end of input "
        case ast of
          Left err -> assertEqual "Unexpected error msg" expectedMsg (formatError err)
          Right _ -> assertFailure "AST should fail parsing"
    --
    describe "Rules can be ignored with inline comments" $ do
      it "ignores single rule" $
        let dockerFile =
              [ "FROM ubuntu",
                "# hadolint ignore=DL3002",
                "USER root"
              ]
         in ruleCatchesNot "DL3002" $ Text.unlines dockerFile
      it "ignores only the given rule" $
        let dockerFile =
              [ "FROM scratch",
                "# hadolint ignore=DL3001",
                "USER root"
              ]
         in ruleCatches "DL3002" $ Text.unlines dockerFile
      it "ignores only the given rule, when multiple passed" $
        let dockerFile =
              [ "FROM scratch",
                "# hadolint ignore=DL3001,DL3002",
                "USER root"
              ]
         in ruleCatchesNot "DL3002" $ Text.unlines dockerFile
      it "ignores the rule only if directly above the instruction" $
        let dockerFile =
              [ "# hadolint ignore=DL3001,DL3002",
                "FROM ubuntu",
                "USER root"
              ]
         in ruleCatches "DL3002" $ Text.unlines dockerFile
      it "won't ignore the rule if passed invalid rule names" $
        let dockerFile =
              [ "FROM scratch",
                "# hadolint ignore=crazy,DL3002",
                "USER root"
              ]
         in ruleCatches "DL3002" $ Text.unlines dockerFile
      it "ignores multiple rules correctly, even with some extra whitespace" $
        let dockerFile =
              [ "FROM node as foo",
                "# hadolint ignore=DL3023, DL3021",
                "COPY --from=foo bar baz ."
              ]
         in do
              ruleCatchesNot "DL3023" $ Text.unlines dockerFile
              ruleCatchesNot "DL3021" $ Text.unlines dockerFile
    --
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

    -- Run tests for the Config module
    ConfigSpec.tests
    -- Run tests for the Shell module
    ShellSpec.tests
    -- Run rule tests
    Shellcheck.tests
    DL3000.tests
    DL3001.tests
    DL3002.tests
    DL3003.tests
    DL3004.tests
    DL3005.tests
    DL3006.tests
    DL3007.tests
    DL3008.tests
    DL3009.tests
    DL3010.tests
    DL3011.tests
    DL3012.tests
    DL3013.tests
    DL3014.tests
    DL3015.tests
    DL3016.tests
    DL3017.tests
    DL3018.tests
    DL3019.tests
    DL3020.tests
    DL3021.tests
    DL3022.tests
    DL3023.tests
    DL3024.tests
    DL3025.tests
    DL3026.tests
    DL3027.tests
    DL3028.tests
    DL3029.tests
    DL3030.tests
    DL3031.tests
    DL3032.tests
    DL3033.tests
    DL3034.tests
    DL3035.tests
    DL3036.tests
    DL3037.tests
    DL3038.tests
    DL3039.tests
    DL3040.tests
    DL3041.tests
    DL3042.tests
    DL3043.tests
    DL3044.tests
    DL3045.tests
    DL3046.tests
    DL3047.tests
    DL3048.tests
    DL3049.tests
    DL3050.tests
    DL3051.tests
    DL3052.tests
    DL3053.tests
    DL3054.tests
    DL3055.tests
    DL3056.tests
    DL3057.tests
    DL3058.tests
    DL4001.tests
    DL4003.tests
    DL4004.tests
    DL4005.tests
    DL4006.tests
