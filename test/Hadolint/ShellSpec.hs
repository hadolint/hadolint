module Hadolint.ShellSpec (spec) where

import Data.Text qualified as Text
import Hadolint.Shell
import Test.Hspec


spec :: SpecWith ()
spec =
  describe "Shell unit tests" $ do
  --
    it "getFlagArgs" $ do
      let cmd = Command "useradd"
                        [ CmdPart "12345" 2,
                          CmdPart "67890" 4,
                          CmdPart "luser" 5 ]
                        [ CmdPart "u" 1,
                          CmdPart "u" 3 ]
       in getFlagArg "u" cmd `shouldBe` [ "12345",
                                          "67890" ]
      let cmd = Command "useradd"
                        [ CmdPart "12345" 2,
                          CmdPart "luser" 3 ]
                        [ CmdPart "u" 1 ]
       in getFlagArg "u" cmd `shouldBe` [ "12345" ]

      let cmd = Command "useradd"
                        [ CmdPart "12345" 2,
                          CmdPart "luser" 3 ]
                        [ CmdPart "u" 1 ]
       in getFlagArg "f" cmd `shouldBe` []
  --
    it "hasFlag" $ do
      let cmd = Command "useradd"
                        [ CmdPart "luser" 1 ]
                        [ CmdPart "l" 0 ]
       in hasFlag "l" cmd `shouldBe` True
      let cmd = Command "useradd"
                        [ CmdPart "luser" 1 ]
                        [ CmdPart "l" 0 ]
       in hasFlag "f" cmd `shouldBe` False
  --
    it "dropFlagArg" $ do
      let cmd = Command "npm"
                        [ CmdPart "verbose" 2,
                          CmdPart "install" 3,
                          CmdPart "bla@1.0.0" 4]
                        [ CmdPart "loglevel" 1]
      let res = Command "npm"
                        [ CmdPart "install" 3,
                          CmdPart "bla@1.0.0" 4]
                        [ CmdPart "loglevel" 1]
       in do
        dropFlagArg ["loglevel"] cmd `shouldBe` res
  --
    it "dropFlagArg keeps the next argument when the flag value is inline" $ do
      let cmds = presentCommands (parseShell "npm --loglevel=info install bla@1.0.0")
      cmds `shouldNotBe` []
      map (dropFlagArg ["loglevel"]) cmds `shouldBe` cmds
  --
    -- Regression tests for hadolint/hadolint#1137: a RUN heredoc fed to a
    -- shell (`RUN <<EOF bash`) is represented by language-docker as the
    -- arguments text @"bash\n<body>"@, so the first line names the
    -- interpreter the body runs under.
    it "heredocShell detects the interpreter of a heredoc RUN" $ do
      heredocShell (Text.pack "bash\nset -euxo pipefail\necho 123 | cat")
        `shouldBe` Just ("bash", "set -euxo pipefail\necho 123 | cat\n")
      heredocShell (Text.pack "/bin/bash\nset -o pipefail\necho 123 | cat")
        `shouldBe` Just ("bash", "set -o pipefail\necho 123 | cat\n")
      heredocShell (Text.pack "echo 123 | cat") `shouldBe` Nothing
      heredocShell (Text.pack "bash") `shouldBe` Nothing
      -- A first line carrying arguments is a command in the body, not an
      -- interpreter line.
      heredocShell (Text.pack "bash -e setup.sh\necho hi") `shouldBe` Nothing
      heredocShell (Text.pack "python\nprint(\"foo\")") `shouldBe` Nothing
  --
    it "heredocShellName maps the interpreter to a ShellCheck dialect" $ do
      heredocShellName (Text.pack "bash\nset -o pipefail\necho 123 | cat")
        `shouldBe` Just "/bin/bash"
      heredocShellName (Text.pack "ksh\nset -o pipefail\necho 123 | cat")
        `shouldBe` Just "/bin/ksh"
      -- zsh is deliberately excluded: ShellCheck cannot check zsh scripts.
      heredocShellName (Text.pack "zsh\nset -o pipefail\necho 123 | cat")
        `shouldBe` Nothing
      heredocShellName (Text.pack "echo 123 | cat") `shouldBe` Nothing
  --
    it "heredocSetsPipefail recognises set -o pipefail in a heredoc body" $ do
      let shell = heredocSetsPipefail . parseShell . Text.pack
      shell "bash\n    set -euxo pipefail\n    echo 123 | cat" `shouldBe` True
      shell "bash\n    set -o pipefail\n    echo 123 | cat" `shouldBe` True
      shell "bash\n    set -o errexit -o pipefail\n    echo 123 | cat" `shouldBe` True
      shell "/bin/bash\n    set -euxo pipefail\n    echo 123 | cat" `shouldBe` True
      shell "zsh\n    set -euxo pipefail\n    echo 123 | cat" `shouldBe` True
      -- No `set -o pipefail` in the body.
      shell "bash\n    echo 123 | cat" `shouldBe` False
      -- `set -o pipefail` after the pipe cannot protect it.
      shell "bash\n    echo 123 | cat\n    set -euxo pipefail" `shouldBe` False
      -- A `|` inside quotes is not a pipeline.
      shell "bash\n    echo \"a|b\"\n    set -euxo pipefail\n    echo 123 | cat" `shouldBe` True
      -- `sh` has no pipefail, and a heredoc without an interpreter runs
      -- under the image shell, so neither counts.
      shell "sh\n    set -euxo pipefail\n    echo 123 | cat" `shouldBe` False
      shell "set -euxo pipefail\necho 123 | cat" `shouldBe` False
