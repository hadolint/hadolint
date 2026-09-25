module RegressionSpec (spec) where

import Data.Default
import qualified Data.Text as Text
import Hadolint (Configuration (..))
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

    it "`ARG` or `ENV` does not reset shell name given by SHELL instruction or shell pragma" $ do
      let ?config = def { ignoreRules = [ "DL3057" ] }
      let dockerfile =
            Text.unlines
              [ "# escape=`",
                "# hadolint shell=powershell",
                "",
                "ARG WINDOWS_VERSION=ltsc2022",
                "FROM mcr.microsoft.com/windows/servercore:\"${WINDOWS_VERSION}\" AS jre-and-war",
                "# $ProgressPreference: https://github.com/PowerShell/PowerShell/issues/2138#issuecomment-251261324",
                "SHELL [\"powershell\", \"-Command\", \"$ErrorActionPreference = 'Stop'; $ProgressPreference = 'SilentlyContinue';\"]",
                "",
                "ARG JAVA_ZIP_URL=\"Provided by docker-bake.hcl\"",
                "ARG jdkTemp='C:\\jdktemp'",
                "ARG localArchive=\"${jdkTemp}\\jdk.zip\"",
                "RUN New-Item -ItemType Directory -Path $env:jdkTemp | Out-Null ; `",
                "    Invoke-WebRequest $env:JAVA_ZIP_URL -OutFile $env:localArchive"
              ]
       in do
        assertChecks dockerfile passesShellcheck
        assertChecks dockerfile passesAllEnabled
