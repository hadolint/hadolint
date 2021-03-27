module Shellcheck (tests) where

import Data.Text as Text
import Helpers
import Test.Hspec


tests :: SpecWith ()
tests = do
  let ?rulesConfig = mempty
  describe "Shellcheck" $ do
    it "runs shellchek on RUN instructions" $ do
      assertChecks "RUN echo $MISSING_QUOTES" failsShellcheck
      assertOnBuildChecks "RUN echo $MISSING_QUOTES" failsShellcheck
    it "not warns on valid scripts" $ do
      assertChecks "RUN echo foo" passesShellcheck
      assertOnBuildChecks "RUN echo foo" passesShellcheck

    it "Does not complain on default env vars" $
      let dockerFile =
            Text.unlines
              [ "RUN echo \"$HTTP_PROXY\"",
                "RUN echo \"$http_proxy\"",
                "RUN echo \"$HTTPS_PROXY\"",
                "RUN echo \"$https_proxy\"",
                "RUN echo \"$FTP_PROXY\"",
                "RUN echo \"$ftp_proxy\"",
                "RUN echo \"$NO_PROXY\"",
                "RUN echo \"$no_proxy\""
              ]
       in do
            assertChecks dockerFile passesShellcheck
            assertOnBuildChecks dockerFile passesShellcheck

    it "Complain on missing env vars" $
      let dockerFile =
            Text.unlines
              [ "RUN echo \"$RTTP_PROXY\""
              ]
       in do
            assertChecks dockerFile failsShellcheck
            assertOnBuildChecks dockerFile failsShellcheck

    it "Is aware of ARGS and ENV" $
      let dockerFile =
            Text.unlines
              [ "ARG foo=bar",
                "ARG another_foo",
                "ENV bar=10 baz=20",
                "RUN echo \"$foo\"",
                "RUN echo \"$another_foo\"",
                "RUN echo \"$bar\"",
                "RUN echo \"$baz\""
              ]
       in do
            assertChecks dockerFile passesShellcheck
            assertOnBuildChecks dockerFile failsShellcheck

    it "Resets env vars after a FROM" $
      let dockerFile =
            Text.unlines
              [ "ARG foo=bar",
                "ARG another_foo",
                "ENV bar=10 baz=20",
                "FROM debian",
                "RUN echo \"$foo\""
              ]
       in do
            assertChecks dockerFile failsShellcheck
            assertOnBuildChecks dockerFile failsShellcheck

    it "Defaults the shell to sh" $
      let dockerFile =
            Text.unlines
              [ "RUN echo $RANDOM"
              ]
       in do
            assertChecks dockerFile failsShellcheck
            assertOnBuildChecks dockerFile failsShellcheck

    it "Can change the shell check to bash" $
      let dockerFile =
            Text.unlines
              [ "SHELL [\"/bin/bash\", \"-eo\", \"pipefail\", \"-c\"]",
                "RUN echo $RANDOM"
              ]
       in do
            assertChecks dockerFile passesShellcheck
            -- This is debatable, as it should actaully pass, but detecting it correctly
            -- is quite difficult
            assertOnBuildChecks dockerFile failsShellcheck
    it "Resets the SHELL to sh after a FROM" $
      let dockerFile =
            Text.unlines
              [ "SHELL [\"/bin/bash\", \"-eo\", \"pipefail\", \"-c\"]",
                "FROM debian",
                "RUN echo $RANDOM"
              ]
       in do
            assertChecks dockerFile failsShellcheck
            assertOnBuildChecks dockerFile failsShellcheck

    it "Does not complain on ash shell" $
      let dockerFile =
            Text.unlines
              [ "SHELL [\"/bin/ash\", \"-o\", \"pipefail\", \"-c\"]",
                "RUN echo hello"
              ]
       in do
            assertChecks dockerFile passesShellcheck
            assertOnBuildChecks dockerFile passesShellcheck

    it "Does not complain on powershell" $
      let dockerFile =
            Text.unlines
              [ "SHELL [\"pwsh\", \"-c\"]",
                "RUN Get-Variable PSVersionTable | Select-Object -ExpandProperty Value"
              ]
       in do
            assertChecks dockerFile passesShellcheck
            assertOnBuildChecks dockerFile passesShellcheck
