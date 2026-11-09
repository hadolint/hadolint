module Hadolint.Shell where

import Control.Monad.Writer (Writer, execWriter, tell)
import Data.Functor.Identity (runIdentity)
import Data.List (findIndex, isInfixOf)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import ShellCheck.AST
  ( Id (..),
    Token (..),
    pattern TA_Expansion,
    pattern TA_Sequence,
    pattern T_Annotation,
    pattern T_Backticked,
    pattern T_DollarArithmetic,
    pattern T_DollarBraced,
    pattern T_DollarExpansion,
    pattern T_DollarSingleQuoted,
    pattern T_DoubleQuoted,
    pattern T_Glob,
    pattern T_Literal,
    pattern T_NormalWord,
    pattern T_ParamSubSpecialChar,
    pattern T_Pipe,
    pattern T_Pipeline,
    pattern T_Redirecting,
    pattern T_SimpleCommand,
    pattern T_SingleQuoted,
  )
import ShellCheck.AST qualified
import ShellCheck.ASTLib qualified
import ShellCheck.Checker (checkScript)
import ShellCheck.Interface
import ShellCheck.Parser qualified

data CmdPart = CmdPart
  { arg :: Text,
    partId :: Int
  }
  deriving (Eq, Show)

data Command = Command
  { name :: Text.Text,
    arguments :: [CmdPart],
    flags :: [CmdPart]
  }
  deriving (Eq, Show)

data ParsedShell = ParsedShell
  { original :: Text.Text,
    parsed :: ParseResult,
    presentCommands :: [Command]
  }

instance Show ParsedShell where
  show p = show (presentCommands p)

data ShellOpts = ShellOpts
  { shellName :: Text.Text,
    envVars :: Set.Set Text.Text
  }

defaultShellOpts :: ShellOpts
defaultShellOpts = ShellOpts "/bin/sh -c" defaultVars
  where
    defaultVars =
      Set.fromList
        [ "HTTP_PROXY",
          "http_proxy",
          "HTTPS_PROXY",
          "https_proxy",
          "FTP_PROXY",
          "ftp_proxy",
          "NO_PROXY",
          "no_proxy"
        ]

addVars :: [Text.Text] -> ShellOpts -> ShellOpts
addVars vars (ShellOpts n v) = ShellOpts n (v <> Set.fromList vars)

setShell :: Text.Text -> ShellOpts -> ShellOpts
setShell s (ShellOpts _ v) = ShellOpts s v

shellcheck :: ShellOpts -> ParsedShell -> [PositionedComment]
shellcheck (ShellOpts sh env) (ParsedShell txt _ _) =
  if any (`Text.isInfixOf` shell) nonPosixShells || hasUnsupportedShebang txt
    then [] -- Do no run for non-posix shells i.e. powershell, cmd.exe
    else runShellCheck
  where
    -- A heredoc RUN fed to a shell (e.g. `RUN <<EOF bash`) runs its body
    -- under that shell, not under the image SHELL, so check the body with
    -- that shell's dialect. Otherwise `set -o pipefail` in a bash heredoc
    -- is wrongly flagged SC3040 ("In POSIX sh, set option pipefail is
    -- undefined").
    shell = fromMaybe sh (heredocShellName txt)
    runShellCheck = crComments $ runIdentity $ checkScript si spec
    si = mockedSystemInterface [("", "")]
    spec =
      emptyCheckSpec
        { csFilename = "", -- filename can be omitted because we only want the parse results back
          csScript = script,
          csCheckSourced = False,
          csExcludedWarnings = exclusions,
          csShellTypeOverride = Nothing,
          csMinSeverity = StyleC
        }
    script = Text.unpack $ "#!" <> extractShell shell <> "\n" <> printVars <> txt
    exclusions =
      [ 2187, -- exclude the warning about the ash shell not being supported
        1090, -- requires a directive (shell comment) that can't be expressed in a Dockerfile
        1091 -- requires a directive (shell comment) that can't be expressed in a Dockerfile
      ]

    extractShell s = fromMaybe "" (listToMaybe . Text.words $ s)
    printVars = Text.unlines . Set.toList $ Set.map (\v -> "export " <> v <> "=1") env

nonPosixShells :: [Text.Text]
nonPosixShells = ["pwsh", "powershell", "cmd"]

-- | Shells that a RUN heredoc may be fed to, by the name that may follow
-- the heredoc marker (e.g. @RUN <<EOF bash@). language-docker represents
-- such a RUN as the arguments text @"bash\n<body>"@: the first line names
-- the interpreter the heredoc is fed to, and the rest is the script it
-- runs.
heredocShells :: [Text.Text]
heredocShells = ["bash", "sh", "dash", "ash", "zsh", "ksh"]

-- | If a RUN's arguments text is a heredoc fed to a shell, return that
-- shell's basename and the script body it runs. The first line must be
-- exactly a shell name (optionally as an absolute path); a first line
-- carrying arguments (e.g. @bash -e setup.sh@ as the first line of a plain
-- heredoc body) is a command in the body, not an interpreter line, and is
-- left alone.
heredocShell :: Text.Text -> Maybe (Text.Text, Text.Text)
heredocShell txt =
  case Text.lines txt of
    (first : rest)
      | not (null rest)
      , shell `elem` heredocShells ->
          Just (shell, Text.unlines rest)
      where
        shell = shellBasename (Text.strip first)
    _ -> Nothing

-- | The shell ShellCheck should check a heredoc body as, given the
-- interpreter the heredoc is fed to. @zsh@ is deliberately excluded:
-- ShellCheck cannot check zsh scripts (it reports SC1071), so a zsh
-- heredoc keeps the image shell instead of erroring on every line.
-- @sh@, @dash@ and @ash@ map to the shells ShellCheck already treats as
-- POSIX sh; @bash@ and @ksh@ keep their own dialect, in which
-- @set -o pipefail@ is defined.
heredocShellName :: Text.Text -> Maybe Text.Text
heredocShellName txt =
  case heredocShell txt of
    Just ("bash", _) -> Just "/bin/bash"
    Just ("ksh", _) -> Just "/bin/ksh"
    Just ("ash", _) -> Just "/bin/ash"
    Just ("sh", _) -> Just "/bin/sh"
    Just ("dash", _) -> Just "/bin/dash"
    _ -> Nothing

-- | Shells in which @set -o pipefail@ is defined, so a heredoc body that
-- enables it before its first pipe makes DL4006's warning unnecessary.
-- This mirrors the shells DL4006 already recognises on SHELL
-- instructions, plus @ksh@, which also supports the option.
pipefailShells :: [Text.Text]
pipefailShells = ["bash", "zsh", "ash", "ksh"]

-- | Whether a RUN fed to a shell as a heredoc enables @pipefail@ in its own
-- body before the first pipe (e.g. @RUN <<EOF bash@ with
-- @set -euxo pipefail@ at the top of the body). Only the shell the heredoc
-- runs under can provide @pipefail@: an @sh@ heredoc, or a heredoc without
-- an interpreter (which runs under the image shell), does not count, so
-- @set -o pipefail@ there still warns.
heredocSetsPipefail :: ParsedShell -> Bool
heredocSetsPipefail (ParsedShell txt _ _) =
  case heredocShell txt of
    Just (shell, body)
      | shell `elem` pipefailShells -> setsPipefailBeforePipe body
    _ -> False

-- | Whether a script enables @pipefail@ with @set@ before its first
-- pipeline. The @set@ must come first: a @set -o pipefail@ after a pipe
-- cannot protect that pipe.
setsPipefailBeforePipe :: Text.Text -> Bool
setsPipefailBeforePipe body =
  case (setLine, pipeLine) of
    (Just s, Just p) -> s < p
    _ -> False
  where
    ls = Text.lines body
    setLine = findIndex enablesPipefail ls
    pipeLine = findIndex hasPipeline ls

-- | Whether a line is a @set@ command that enables @pipefail@, e.g.
-- @set -o pipefail@ or @set -euxo pipefail@.
enablesPipefail :: Text.Text -> Bool
enablesPipefail line =
  case Text.words (Text.stripStart line) of
    ("set" : args) -> enables args
    _ -> False
  where
    enables [] = False
    enables ("--" : _) = False
    enables (flag : rest)
      -- @-o@ takes its option name as the next argument.
      | flag == "-o" = case rest of
          ("pipefail" : _) -> True
          (_ : rest') -> enables rest'
          [] -> False
      -- A combined short-flag token: if it contains @o@, the option name
      -- is the rest of the token (@-opipefail@) or, when @o@ is the last
      -- letter (@-euxo@), the next argument.
      | Just optName <- oOption flag =
          if Text.null optName
            then case rest of
              ("pipefail" : _) -> True
              (_ : rest') -> enables rest'
              [] -> False
            else optName == "pipefail" || enables rest
      -- Any other @-...@ token is a flag without an attached option name.
      | "-" `Text.isPrefixOf` flag = enables rest
      -- A bare word ends the flags: @set@ from here on only sets
      -- positional parameters, so @pipefail@ cannot follow as an option.
      | otherwise = False
    -- The option name a combined flag token passes to @-o@, if any:
    -- @-euxo@ -> @""@ (the name is the next argument);
    -- @-opipefail@ -> @"pipefail"@.
    oOption f
      | "-" `Text.isPrefixOf` f
      , not ("--" `Text.isPrefixOf` f)
      , Text.length f > 2
      , Text.any (== 'o') (Text.drop 1 f) =
          Just (Text.drop 1 (Text.dropWhile (/= 'o') f))
      | otherwise = Nothing

-- | Whether a line contains a pipeline operator: a @|@ outside quotes that
-- is not part of @||@. A @#@ outside quotes starts a comment, so a @|@
-- after one does not count.
hasPipeline :: Text.Text -> Bool
hasPipeline line = scan 'n' True (Text.unpack line)
  where
    -- quote state ('n' none, 's' single, 'd' double) and whether the
    -- previous character was a word boundary (line start or blank).
    scan _ _ [] = False
    scan 's' _ ('\'' : cs) = scan 'n' False cs
    scan 's' _ (_ : cs) = scan 's' False cs
    scan 'd' _ ('"' : cs) = scan 'n' False cs
    scan 'd' _ ('\\' : _ : cs) = scan 'd' False cs
    scan 'd' _ (_ : cs) = scan 'd' False cs
    scan 'n' _ ('\'' : cs) = scan 's' False cs
    scan 'n' _ ('"' : cs) = scan 'd' False cs
    scan 'n' _ ('\\' : _ : cs) = scan 'n' False cs
    scan 'n' True ('#' : _) = False
    scan 'n' _ ('|' : '|' : cs) = scan 'n' False cs
    scan 'n' _ ('|' : _) = True
    scan 'n' _ (c : cs) = scan 'n' (c == ' ' || c == '\t') cs

-- | Returns the last path segment of a shell command name, so that
-- "/usr/bin/bash" and "/bin/bash" both resolve to "bash" just like a bare
-- "bash" would. This lets shell-name matching (e.g. detecting bash/zsh/ash
-- for the "-o pipefail" check) work regardless of which absolute path a
-- shell happens to be installed under.
shellBasename :: Text.Text -> Text.Text
shellBasename = last . Text.splitOn "/"

hasUnsupportedShebang :: Text.Text -> Bool
hasUnsupportedShebang script =
  "#!" `Text.isPrefixOf` script
    && not
      ( any
          (`Text.isPrefixOf` script)
          [ "#!/bin/sh",
            "#!/bin/bash",
            "#!/bin/ksh",
            "#!/usr/bin/env sh",
            "#!/usr/bin/env bash",
            "#!/usr/bin/env ksh"
          ]
      )

parseShell :: Text.Text -> ParsedShell
parseShell txt = ParsedShell {original = txt, parsed = parsedResult, presentCommands = commands}
  where
    parsedResult =
      runIdentity $
        ShellCheck.Parser.parseScript
          (mockedSystemInterface [("", "")])
          newParseSpec
            { psFilename = "", -- There is no filename
              psScript = Text.unpack $ "#!/bin/bash\n" <> txt,
              psCheckSourced = False
            }

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

anyCommands :: (Command -> Bool) -> ParsedShell -> Bool
anyCommands check script = any check (presentCommands script)

findCommandIndex :: (Command -> Bool) -> ParsedShell -> Maybe Int
findCommandIndex check script = findIndex check (presentCommands script)

findCommandNames :: ParsedShell -> [Text]
findCommandNames script = map name (presentCommands script)

cmdHasArgs :: Text.Text -> [Text.Text] -> Command -> Bool
cmdHasArgs expectedName = cmdsHaveArgs [expectedName]

cmdsHaveArgs :: [Text.Text] -> [Text.Text] -> Command -> Bool
cmdsHaveArgs expectedNames expectedArgs (Command n args _)
  | n `notElem` expectedNames = False
  | otherwise = not $ null [arg | CmdPart arg _ <- args, arg `elem` expectedArgs]

cmdHasAllArgs :: Text.Text -> [Text.Text] -> Command -> Bool
cmdHasAllArgs expectedName = cmdsHaveAllArgs [expectedName]

cmdsHaveAllArgs :: [Text.Text] -> [Text.Text] -> Command -> Bool
cmdsHaveAllArgs expectedNames expectedArgs (Command n args _)
  | n `notElem` expectedNames = False
  | otherwise = all (`elem` [arg | CmdPart arg _ <- args]) expectedArgs

-- | Whether the command is an @rm@ that recursively removes one of the given
-- paths, however the flags are written (@-rf@, @-fr@, @-r -f@, @--recursive@).
isRecursiveRemoval :: [Text.Text] -> Command -> Bool
isRecursiveRemoval paths cmd@(Command n _ _) =
  n == "rm"
    && hasAnyFlag ["r", "R", "recursive"] cmd
    && any (`elem` paths) (getArgsNoFlags cmd)

cmdHasPrefixArg :: Text.Text -> Text.Text -> Command -> Bool
cmdHasPrefixArg expectedName expectedArg (Command n args _)
  | expectedName /= n = False
  | otherwise = not $ null [arg | CmdPart arg _ <- args, expectedArg `Text.isPrefixOf` arg]

extractAllArgs :: Token -> [CmdPart]
extractAllArgs (T_SimpleCommand _ _ (_ : allArgs)) = map mkPart allArgs
  where
    mkPart token =
      CmdPart
        (Text.concat . fmap Text.pack $ simplify token)
        (mkId (ShellCheck.AST.getId token))
    mkId (Id i) = i
extractAllArgs _ = []

-- Modified version of ShellCheck.ASTLib.oversimplify
-- This version keeps variable names
simplify :: Token -> [String]
simplify token =
  case token of
    (T_NormalWord _ l) -> [concat (concatMap simplify l)]
    (T_DoubleQuoted _ l) -> [concat (concatMap simplify l)]
    (T_SingleQuoted _ s) -> [s]
    (T_DollarBraced _ _ t) -> ["${" ++ concat (simplify t) ++ "}"]
    (T_DollarArithmetic _ _) -> ["${VAR}"]
    (T_DollarExpansion _ t) -> ["${" ++ concat (concatMap simplify t) ++ "}"]
    (T_Backticked _ _) -> ["${VAR}"]
    (T_Glob _ s) -> [s]
    (T_Pipeline _ _ [x]) -> simplify x
    (T_Literal _ x) -> [x]
    (T_ParamSubSpecialChar _ x) -> [x]
    (T_SimpleCommand _ _ wds) -> concatMap simplify wds
    (T_Redirecting _ _ foo) -> simplify foo
    (T_DollarSingleQuoted _ s) -> [s]
    (T_Annotation _ _ s) -> simplify s
    (TA_Sequence _ [TA_Expansion _ v]) -> concatMap simplify v
    _ -> []

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
hasFlag flag cmd = countFlag flag cmd > 0

countFlag :: Text.Text -> Command -> Int
countFlag flag Command {flags} = length [f | CmdPart f _ <- flags, f == flag]

hasAnyFlag :: [Text.Text] -> Command -> Bool
hasAnyFlag fs Command {flags} = not $ null [f | CmdPart f _ <- flags, f `elem` fs]

hasArg :: Text.Text -> Command -> Bool
hasArg arg Command {arguments} = not $ null [a | CmdPart a _ <- arguments, a == arg]

dropFlagArg :: [Text.Text] -> Command -> Command
dropFlagArg flagsToDrop Command {name, arguments, flags} = Command name filteredArgs flags
  where
    idsToDrop =
      Set.fromList
        [getValueId fId arguments | CmdPart f fId <- flags, f `elem` flagsToDrop, not (hasInlineValue fId)]
    hasInlineValue fId = or ["=" `Text.isInfixOf` a | CmdPart a aId <- arguments, aId == fId]
    filteredArgs = [arg | arg@(CmdPart _ aId) <- arguments, not (aId `Set.member` idsToDrop)]

-- | given a flag and a command, return list of arguments for that particular
-- flag. E.g., if the command is `useradd -u 12345 luser` and this function is
-- called for the flag `u`, it returns ["12345"].
getFlagArg :: Text.Text -> Command -> [Text.Text]
getFlagArg flag Command {arguments, flags} = extractArgs
  where
    idsToGet = Set.fromList [getValueId fId arguments | CmdPart f fId <- flags, f == flag]
    extractArgs = [arg | (CmdPart arg aId) <- arguments, aId `Set.member` idsToGet]

getValueId :: Int -> [CmdPart] -> Int
getValueId fId flags = foldl min (maxBound :: Int) $ filter (> fId) $ map partId flags

-- | Check if a command contains a program call in the Run instruction
usingProgram :: Text.Text -> ParsedShell -> Bool
usingProgram prog args = not $ null [cmd | cmd <- findCommandNames args, cmd == prog]

isPipInstall :: Command -> Bool
isPipInstall cmd@(Command name _ _) = isStdPipInstall || isPythonPipInstall
  where
    isStdPipInstall =
      "pip" `Text.isPrefixOf` name
        && not ("pipenv" `Text.isPrefixOf` name)
        && ["install"] `isInfixOf` getArgs cmd
    isPythonPipInstall =
      "python" `Text.isPrefixOf` name
        && ["-m", "pip", "install"] `isInfixOf` getArgs cmd
