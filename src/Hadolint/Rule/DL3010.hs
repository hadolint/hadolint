module Hadolint.Rule.DL3010 (rule) where

import Data.Foldable (toList)
import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import Hadolint.Rule
import qualified Hadolint.Shell as Shell
import Language.Docker.Syntax


data Acc
  = Acc
      { archives :: Set.Set (Linenumber, Text.Text),
        extracted :: Set.Set (Linenumber, Text.Text)
      }
  | Empty

rule :: Rule Shell.ParsedShell
rule = veryCustomRule check (emptyState Empty) markFailures
  where
    code = "DL3010"
    severity = DLInfoC
    message = "Use `ADD` for extracting archives into an image"

    check _ _ (From _) = emptyState Empty
    check line st (Copy (CopyArgs srcs tgt) (CopyFlags _ _ _ NoSource)) =
      st |> modify (rememberArchives line srcs tgt)
    check _ st (Run (RunArgs args _))
      | Acc archives _ <- state st,
        ex <- foldArguments (getExtractedArchives archives) args =
          st |> modify (markExtracted ex)
      | otherwise = st
    check _ st _ = st

    markFailures :: State Acc -> Failures
    markFailures (State fails (Acc _ e)) =
      Set.foldl' (Seq.|>) fails (Set.map makeFail e)
    markFailures st = failures st

    makeFail :: (Linenumber, Text.Text) -> CheckFailure
    makeFail (line, _) = CheckFailure {..}
{-# INLINEABLE rule #-}


extractsThisArchive :: (Linenumber, Text.Text) -> Shell.Command -> Bool
extractsThisArchive (_, archive) cmd =
  (isTarExtractCommand cmd || isUnzipCommand cmd) && archive `elem` arguments
  where
    arguments = map basename $ Shell.getArgsNoFlags cmd

getExtractedArchives ::
  Set.Set (Linenumber, Text.Text) ->
  Shell.ParsedShell ->
  Set.Set (Linenumber, Text.Text)
getExtractedArchives archives shell =
  Set.filter
    (\a -> any (extractsThisArchive a) cmds)
    archives
  where
    cmds = Shell.presentCommands shell

isTarExtractCommand :: Shell.Command -> Bool
isTarExtractCommand cmd@(Shell.Command name _ _) =
  name == "tar" && (any longExtractFlags args || any shortExtractFlags args)
  where
    longExtractFlags f = f `elem` ["--extract", "--get"]
    shortExtractFlags f = "-" `Text.isPrefixOf` f && "x" `Text.isInfixOf` f
    args = Shell.getArgs cmd

isUnzipCommand :: Shell.Command -> Bool
isUnzipCommand (Shell.Command name _ _) =
  name `elem`
    [ "unzip",
      "gunzip",
      "bunzip2",
      "unlzma",
      "unxz",
      "zgz",
      "uncompress",
      "zcat",
      "gzcat"
    ]

markExtracted :: Set.Set (Linenumber, Text.Text) -> Acc -> Acc
markExtracted _ Empty = Empty
markExtracted exarcv Acc {archives, extracted} =
  Acc { archives, extracted = Set.union exarcv extracted }

rememberArchives ::
  Linenumber ->
  NonEmpty SourcePath ->
  TargetPath ->
  Acc ->
  Acc
rememberArchives line paths target Empty =
  if isArchive $ unTargetPath target
    then Acc
          { archives = Set.singleton (line, basename $ unTargetPath target),
            extracted = Set.empty
          }
    else Acc
          { archives =
              paths
                & toList
                & map (basename . unSourcePath)
                & Set.fromList
                & Set.filter isArchive
                & Set.map (line,),
            extracted = Set.empty
          }
rememberArchives line paths target Acc {archives, extracted} =
  if isArchive $ unTargetPath target
    then Acc
          { archives =
              Set.insert (line, basename $ unTargetPath target) archives,
            extracted
          }
    else Acc
          { archives =
              paths
                & toList
                & map (basename . unSourcePath)
                & Set.fromList
                & Set.filter isArchive
                & Set.map (line,)
                & Set.union archives,
            extracted
          }

basename :: Text.Text -> Text.Text
basename = Text.takeWhileEnd (\c -> c /= '/' && c /= '\\') . dropQuotes

isArchive :: Text.Text -> Bool
isArchive src =
  any (`Text.isSuffixOf` dropQuotes src) archiveFileFormatExtensions
