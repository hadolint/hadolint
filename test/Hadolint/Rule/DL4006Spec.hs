module Hadolint.Rule.DL4006Spec (spec) where

import Data.Default
import Data.Text as Text
import Helpers
import Test.Hspec


spec :: SpecWith ()
spec = do
  let ?config = def

  describe "DL4006 - Set the `SHELL` option `-o pipefail` before RUN with a pipe in it" $ do
    it "warn on missing pipefail" $
      let dockerFile =
            [ "FROM scratch",
              "RUN wget -O - https://some.site | wc -l > /number"
            ]
       in ruleCatches "DL4006" $ Text.unlines dockerFile
    it "don't warn on commands with no pipes" $
      let dockerFile =
            [ "FROM scratch as build",
              "RUN wget -O - https://some.site && wc -l file > /number"
            ]
       in ruleCatchesNot "DL4006" $ Text.unlines dockerFile
    it "don't warn on commands with pipes and the pipefail option" $
      let dockerFile =
            [ "FROM scratch as build",
              "SHELL [\"/bin/bash\", \"-eo\", \"pipefail\", \"-c\"]",
              "RUN wget -O - https://some.site | wc -l file > /number"
            ]
       in ruleCatchesNot "DL4006" $ Text.unlines dockerFile
    it "don't warn on commands with pipes and the pipefail option 2" $
      let dockerFile =
            [ "FROM scratch as build",
              "SHELL [\"/bin/bash\", \"-e\", \"-o\", \"pipefail\", \"-c\"]",
              "RUN wget -O - https://some.site | wc -l file > /number"
            ]
       in ruleCatchesNot "DL4006" $ Text.unlines dockerFile
    it "don't warn on commands with pipes and the pipefail option 3" $
      let dockerFile =
            [ "FROM scratch as build",
              "SHELL [\"/bin/bash\", \"-o\", \"errexit\", \"-o\", \"pipefail\", \"-c\"]",
              "RUN wget -O - https://some.site | wc -l file > /number"
            ]
       in ruleCatchesNot "DL4006" $ Text.unlines dockerFile
    it "don't warn on commands with pipes and the pipefail zsh" $
      let dockerFile =
            [ "FROM scratch as build",
              "SHELL [\"/bin/zsh\", \"-o\", \"pipefail\", \"-c\"]",
              "RUN wget -O - https://some.site | wc -l file > /number"
            ]
       in ruleCatchesNot "DL4006" $ Text.unlines dockerFile
    it "don't warn on powershell" $
      let dockerFile =
            [ "FROM scratch as build",
              "SHELL [\"pwsh\", \"-c\"]",
              "RUN Get-Variable PSVersionTable | Select-Object -ExpandProperty Value"
            ]
       in ruleCatchesNot "DL4006" $ Text.unlines dockerFile
    it "warns when using plain sh" $
      let dockerFile =
            [ "FROM scratch as build",
              "SHELL [\"/bin/sh\", \"-o\", \"pipefail\", \"-c\"]",
              "RUN wget -O - https://some.site | wc -l file > /number"
            ]
       in ruleCatches "DL4006" $ Text.unlines dockerFile
    it "warn on missing pipefail in the next image" $
      let dockerFile =
            [ "FROM scratch as build",
              "SHELL [\"/bin/bash\", \"-o\", \"pipefail\", \"-c\"]",
              "RUN wget -O - https://some.site | wc -l file > /number",
              "FROM scratch as build2",
              "RUN wget -O - https://some.site | wc -l file > /number"
            ]
       in ruleCatches "DL4006" $ Text.unlines dockerFile
    it "warn on missing pipefail if next SHELL is not using it" $
      let dockerFile =
            [ "FROM scratch as build",
              "SHELL [\"/bin/bash\", \"-o\", \"pipefail\", \"-c\"]",
              "RUN wget -O - https://some.site | wc -l file > /number",
              "SHELL [\"/bin/sh\", \"-c\"]",
              "RUN wget -O - https://some.site | wc -l file > /number"
            ]
       in ruleCatches "DL4006" $ Text.unlines dockerFile
    it "ignore non posix shells: pwsh" $
      let dockerFile =
            [ "FROM mcr.microsoft.com/powershell:ubuntu-16.04",
              "SHELL [ \"pwsh\", \"-c\" ]",
              "RUN Get-Variable PSVersionTable | Select-Object -ExpandProperty Value"
            ]
       in ruleCatchesNot "DL4006" $ Text.unlines dockerFile
    it "ignore non posix shells: powershell" $
      let dockerFile =
            [ "FROM mcr.microsoft.com/powershell:ubuntu-16.04",
              "SHELL [ \"powershell.exe\" ]",
              "RUN Get-Variable PSVersionTable | Select-Object -ExpandProperty Value"
            ]
       in ruleCatchesNot "DL4006" $ Text.unlines dockerFile
    it "ignore non posix shells: cmd.exe" $
      let dockerFile =
            [ "FROM mcr.microsoft.com/powershell:ubuntu-16.04",
              "SHELL [ \"cmd.exe\", \"/c\" ]",
              "RUN Get-Variable PSVersionTable | Select-Object -ExpandProperty Value"
            ]
       in ruleCatchesNot "DL4006" $ Text.unlines dockerFile
