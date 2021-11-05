module Hadolint.ShellSpec (spec) where

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
