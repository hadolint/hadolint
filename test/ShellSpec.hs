module ShellSpec where

import Data.Text as Text
import Hadolint.Shell
import Test.Hspec

tests :: SpecWith ()
tests =
  describe "Shell unit tests" $ do
  --
    it "getFlagArgs" $ do
      let cmd = Command (Text.pack "useradd")
                        [ CmdPart (Text.pack "12345") 2,
                          CmdPart (Text.pack "67890") 4,
                          CmdPart (Text.pack "luser") 5 ]
                        [ CmdPart (Text.pack "u") 1,
                          CmdPart (Text.pack "u") 3 ]
       in getFlagArg (Text.pack "u") cmd `shouldBe` [ Text.pack "12345",
                                                      Text.pack "67890" ]
      let cmd = Command (Text.pack "useradd")
                        [ CmdPart (Text.pack "12345") 2,
                          CmdPart (Text.pack "luser") 3 ]
                        [ CmdPart (Text.pack "u") 1 ]
       in getFlagArg (Text.pack "u") cmd `shouldBe` [ Text.pack "12345" ]

      let cmd = Command (Text.pack "useradd")
                        [ CmdPart (Text.pack "12345") 2,
                          CmdPart (Text.pack "luser") 3 ]
                        [ CmdPart (Text.pack "u") 1 ]
       in getFlagArg (Text.pack "f") cmd `shouldBe` []
  --
    it "hasFlag" $ do
      let cmd = Command (Text.pack "useradd")
                        [ CmdPart (Text.pack "luser") 1 ]
                        [ CmdPart (Text.pack "l") 0 ]
       in hasFlag (Text.pack "l") cmd `shouldBe` True
      let cmd = Command (Text.pack "useradd")
                        [ CmdPart (Text.pack "luser") 1 ]
                        [ CmdPart (Text.pack "l") 0 ]
       in hasFlag (Text.pack "f") cmd `shouldBe` False
