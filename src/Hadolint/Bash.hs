module Hadolint.Bash where

import Control.Monad.Writer (Writer, execWriter, tell)
import Data.Functor.Identity (runIdentity)
import Data.List (nub)
import Data.Maybe (mapMaybe)
import qualified Data.Text as Text
import qualified ShellCheck.AST
import ShellCheck.AST (Id(..), Token(..))
import qualified ShellCheck.ASTLib
import ShellCheck.Checker
import ShellCheck.Interface
import qualified ShellCheck.Parser

data ParsedBash = ParsedBash
    { original :: Text.Text
    , parsed :: ParseResult
    }

shellcheck :: ParsedBash -> [Comment]
shellcheck (ParsedBash txt _) = map comment $ crComments $ runIdentity $ checkScript si spec
  where
    comment (PositionedComment _ _ c) = c
    si = mockedSystemInterface [("", "")]
    spec = CheckSpec filename script sourced exclusions (Just Bash)
    script = "#!/bin/bash\n" ++ Text.unpack txt
    filename = "" -- filename can be ommited because we only want the parse results back
    sourced = False
    exclusions = []

parseShell :: Text.Text -> ParsedBash
parseShell txt =
    ParsedBash
    { original = txt
    , parsed =
          runIdentity $
          ShellCheck.Parser.parseScript
              (mockedSystemInterface [("", "")])
              ParseSpec
              { psFilename = "" -- There is no filename
              , psScript = "#!/bin/bash\n" ++ Text.unpack txt
              , psCheckSourced = False
              }
    }

findCommands :: ParsedBash -> [Token]
findCommands (ParsedBash _ ast) =
    case prRoot ast of
        Nothing -> []
        Just script -> nub . execWriter $ ShellCheck.AST.doAnalysis extract script
  where
    extract :: Token -> Writer [Token] ()
    extract t =
        case ShellCheck.ASTLib.getCommand t of
            Nothing -> return ()
            Just cmd -> tell [cmd]

allCommands :: (Token -> Bool) -> ParsedBash -> Bool
allCommands check script = all check (findCommands script)

noCommands :: (Token -> Bool) -> ParsedBash -> Bool
noCommands check = allCommands (not . check)

getCommandName :: Token -> Maybe String
getCommandName = ShellCheck.ASTLib.getCommandName

findCommandNames :: ParsedBash -> [String]
findCommandNames = mapMaybe getCommandName . findCommands

cmdHasArgs :: String -> [String] -> Token -> Bool
cmdHasArgs command arguments token@T_SimpleCommand {}
    | ShellCheck.ASTLib.getCommandName token /= Just command = False
    | otherwise = not $ null [arg | arg <- getAllArgs token, arg `elem` arguments]
cmdHasArgs _ _ _ = False

getAllArgs :: Token -> [String]
getAllArgs (T_SimpleCommand _ _ (_:allArgs)) = concatMap ShellCheck.ASTLib.oversimplify allArgs
getAllArgs _ = []

getArgsNoFlags :: Token -> [String]
getArgsNoFlags cmd@(T_SimpleCommand _ _ (_:allArgs)) = concatMap ShellCheck.ASTLib.oversimplify args
  where
    flags = [t | (t, _) <- getAllFlags cmd]
    args = [a | a <- allArgs, a `notElem` flags]
getArgsNoFlags _ = []

getAllFlags :: Token -> [(Token, String)]
getAllFlags cmd@T_SimpleCommand {} = [(t, f) | (t, f) <- ShellCheck.ASTLib.getAllFlags cmd, f /= ""]
getAllFlags _ = []

hasFlag :: String -> Token -> Bool
hasFlag flag = any (\(_, f) -> f == flag) . getAllFlags

dropFlagArg :: [String] -> Token -> Token
dropFlagArg flags cmd@(T_SimpleCommand cid b allArgs) = T_SimpleCommand cid b filterdArgs
  where
    filterdArgs = [arg | arg <- allArgs, isNotNextToken arg]
    isNotNextToken arg = ShellCheck.AST.getId arg `notElem` findTokensToDrop
    findTokensToDrop = [next (ShellCheck.AST.getId t) | (t, f) <- getAllFlags cmd, f `elem` flags]
    next (Id i) = Id (i + 2)
dropFlagArg _ token = token
