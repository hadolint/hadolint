{-# LANGUAGE OverloadedStrings #-}

module Hadolint.Bash where

import Control.Monad.Writer (Writer, execWriter, tell)
import Data.Functor.Identity (runIdentity)
import Data.List (nub)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Semigroup ((<>))
import qualified Data.Set as Set
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

data ShellOpts = ShellOpts
    { shellName :: Text.Text
    , envVars :: Set.Set Text.Text
    }

defaultShellOpts :: ShellOpts
defaultShellOpts = ShellOpts "/bin/sh -c" defaultVars
  where
    defaultVars =
        Set.fromList
            [ "HTTP_PROXY"
            , "http_proxy"
            , "HTTPS_PROXY"
            , "https_proxy"
            , "FTP_PROXY"
            , "ftp_proxy"
            , "NO_PROXY"
            , "no_proxy"
            ]

addVars :: [Text.Text] -> ShellOpts -> ShellOpts
addVars vars (ShellOpts n v) = ShellOpts n (v <> Set.fromList vars)

setShell :: Text.Text -> ShellOpts -> ShellOpts
setShell s (ShellOpts _ v) = ShellOpts s v

shellcheck :: ShellOpts -> ParsedBash -> [Comment]
shellcheck (ShellOpts sh env) (ParsedBash txt _) = map comment runShellCheck
  where
    runShellCheck = crComments $ runIdentity $ checkScript si spec
    comment (PositionedComment _ _ c) = c
    si = mockedSystemInterface [("", "")]
    spec = CheckSpec filename script sourced exclusions Nothing
    script = "#!" ++ extractShell sh ++ "\n" ++ printVars ++ Text.unpack txt
    filename = "" -- filename can be ommited because we only want the parse results back
    sourced = False
    exclusions =
        [ 2187 -- exclude the warning about the ash shell not being supported
        ]
    -- | Shellcheck complains when the shebang has more than one argument, so we only take the first
    extractShell s =
        case listToMaybe . Text.words $ s of
            Nothing -> ""
            Just shell -> Text.unpack shell
    -- | Inject all the collected env vars as exported variables so they can be used
    printVars = Text.unpack . Text.unlines . Set.toList $ Set.map (\v -> "export " <> v <> "=1") env

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

extractTokensWith :: (Token -> Maybe Token) -> ParsedBash -> [Token]
extractTokensWith extractor (ParsedBash _ ast) =
    case prRoot ast of
        Nothing -> []
        Just script -> nub . execWriter $ ShellCheck.AST.doAnalysis extract script
  where
    extract :: Token -> Writer [Token] ()
    extract token =
        case extractor token of
            Nothing -> return ()
            Just t -> tell [t]

findPipes :: ParsedBash -> [Token]
findPipes = extractTokensWith pipesExtractor
  where
    pipesExtractor pipe@T_Pipe {} = Just pipe
    pipesExtractor _ = Nothing

hasPipes :: ParsedBash -> Bool
hasPipes = not . null . findPipes

findCommands :: ParsedBash -> [Token]
findCommands = extractTokensWith commandsExtractor
  where
    commandsExtractor = ShellCheck.ASTLib.getCommand

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
