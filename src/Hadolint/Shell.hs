{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hadolint.Shell where

import Control.Monad.Writer (Writer, execWriter, tell)
import Data.Functor.Identity (runIdentity)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Semigroup ((<>))
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Text (Text)
import qualified ShellCheck.AST
import ShellCheck.AST (Id(..), Token(..))
import qualified ShellCheck.ASTLib
import ShellCheck.Checker
import ShellCheck.Interface
import qualified ShellCheck.Parser

data CmdPart = CmdPart
    { arg :: !Text
    , partId :: !Int
    } deriving (Show)

data Command = Command
    { name :: !Text.Text
    , arguments :: [CmdPart]
    , flags :: [CmdPart]
    } deriving (Show)

data ParsedShell = ParsedShell
    { original :: !Text.Text
    , parsed :: !ParseResult
    , presentCommands :: ![Command]
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

shellcheck :: ShellOpts -> ParsedShell -> [PositionedComment]
shellcheck (ShellOpts sh env) (ParsedShell txt _ _) =
    if "pwsh" `Text.isPrefixOf` sh
        then [] -- Do no run for powershell
        else runShellCheck
  where
    runShellCheck = crComments $ runIdentity $ checkScript si spec
    si = mockedSystemInterface [("", "")]
    spec =
        emptyCheckSpec
            { csFilename = "" -- filename can be ommited because we only want the parse results back
            , csScript = script
            , csCheckSourced = False
            , csExcludedWarnings = exclusions
            , csShellTypeOverride = Nothing
            , csMinSeverity = StyleC
            }
    script = "#!" ++ extractShell sh ++ "\n" ++ printVars ++ Text.unpack txt
    exclusions =
        [ 2187 -- exclude the warning about the ash shell not being supported
        , 1090 -- requires a directive (shell comment) that can't be expressed in a Dockerfile
        ]
    -- | Shellcheck complains when the shebang has more than one argument, so we only take the first
    extractShell s =
        maybe "" Text.unpack (listToMaybe . Text.words $ s)
    -- | Inject all the collected env vars as exported variables so they can be used
    printVars = Text.unpack . Text.unlines . Set.toList $ Set.map (\v -> "export " <> v <> "=1") env

parseShell :: Text.Text -> ParsedShell
parseShell txt = ParsedShell {original = txt, parsed = parsedResult, presentCommands = commands}
  where
    parsedResult =
        runIdentity $
        ShellCheck.Parser.parseScript
            (mockedSystemInterface [("", "")])
            newParseSpec
                { psFilename = "" -- There is no filename
                , psScript = "#!/bin/bash\n" ++ Text.unpack txt
                , psCheckSourced = False
                }
    -- | Extract all commands with their name
    commands = mapMaybe extractNames (findCommandsInResult parsedResult)
    extractNames token =
        case ShellCheck.ASTLib.getCommandName token of
            Nothing -> Nothing
            Just n -> Just $ Command (Text.pack n) allArgs (getAllFlags allArgs)
      where
        allArgs = extractAllArgs token

findCommandsInResult :: ParseResult -> [Token]
findCommandsInResult = extractTokensWith commandsExtractor
  where
    commandsExtractor = ShellCheck.ASTLib.getCommand

extractTokensWith :: forall a. (Token -> Maybe a) -> ParseResult -> [a]
extractTokensWith extractor ast =
    case prRoot ast of
        Nothing -> []
        Just script -> execWriter $ ShellCheck.AST.doAnalysis extract script
  where
    extract :: Token -> Writer [a] ()
    extract token =
        case extractor token of
            Nothing -> return ()
            Just t -> tell [t]

findPipes :: ParsedShell -> [Token]
findPipes (ParsedShell _ ast _) = extractTokensWith pipesExtractor ast
  where
    pipesExtractor pipe@T_Pipe {} = Just pipe
    pipesExtractor _ = Nothing

hasPipes :: ParsedShell -> Bool
hasPipes = not . null . findPipes

allCommands :: (Command -> Bool) -> ParsedShell -> Bool
allCommands check script = all check (presentCommands script)

noCommands :: (Command -> Bool) -> ParsedShell -> Bool
noCommands check = allCommands (not . check)

findCommandNames :: ParsedShell -> [Text]
findCommandNames script = map name (presentCommands script)

cmdHasArgs :: Text.Text -> [Text.Text] -> Command -> Bool
cmdHasArgs expectedName expectedArgs (Command n args _)
    | expectedName /= n = False
    | otherwise = not $ null [arg | CmdPart arg _ <- args, arg `elem` expectedArgs]

cmdHasPrefixArg :: Text.Text -> Text.Text -> Command -> Bool
cmdHasPrefixArg expectedName expectedArg (Command n args _)
    | expectedName /= n = False
    | otherwise = not $ null [arg | CmdPart arg _ <- args, expectedArg `Text.isPrefixOf` arg]

extractAllArgs :: Token -> [CmdPart]
extractAllArgs (T_SimpleCommand _ _ (_:allArgs)) = map mkPart allArgs
  where
    mkPart token =
        CmdPart
            (Text.pack . concat $ ShellCheck.ASTLib.oversimplify token)
            (mkId (ShellCheck.AST.getId token))
    mkId (Id i) = i
extractAllArgs _ = []

getArgs :: Command -> [Text.Text]
getArgs cmd = map arg (arguments cmd)

getAllFlags :: [CmdPart] -> [CmdPart]
getAllFlags = concatMap flag
  where
    flag (CmdPart arg pId)
        | arg == "--" || arg == "-" = []
        | "--" `Text.isPrefixOf` arg = [CmdPart (Text.drop 2 . Text.takeWhile (/= '=') $ arg) pId]
        | "-" `Text.isPrefixOf` arg = map (`CmdPart` pId) (Text.chunksOf 1 (Text.tail arg))
        | otherwise = []

getArgsNoFlags :: Command -> [Text.Text]
getArgsNoFlags args = map arg $ filter (notAFlagId . partId) (arguments args)
  where
    notAFlagId pId = pId `notElem` map partId (flags args)

hasFlag :: Text.Text -> Command -> Bool
hasFlag flag Command {flags} = not $ null [f | CmdPart f _ <- flags, f == flag]

hasAnyFlag :: [Text.Text] -> Command -> Bool
hasAnyFlag fs Command {flags} = not $ null [f | CmdPart f _ <- flags, f `elem` fs]

hasArg :: Text.Text -> Command -> Bool
hasArg arg Command {arguments} = not $ null [a | CmdPart a _ <- arguments, a == arg]

dropFlagArg :: [Text.Text] -> Command -> Command
dropFlagArg flagsToDrop Command {name, arguments, flags} = Command name filterdArgs flags
  where
    idsToDrop = Set.fromList [fId + 2 | CmdPart f fId <- flags, f `elem` flagsToDrop]
    filterdArgs = [arg | arg@(CmdPart _ aId) <- arguments, not (aId `Set.member` idsToDrop)]
