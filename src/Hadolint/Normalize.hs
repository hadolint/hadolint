module Hadolint.Normalize (
 normalizeEscapedLines
) where

import Data.List (intercalate)
import Data.List.Split (splitOn)
import Debug.Trace

escapePlaceHolder = "\\\\"
escapeSeq = "\\\n"

replace old new = intercalate new . splitOn old
count s x = length (splitOn x s) - 1

trimLines :: String -> String
trimLines s = unlines $ map strip $ lines s
  where
    strip = lstrip . rstrip
    lstrip = dropWhile (`elem` " \t")
    rstrip = reverse . lstrip . reverse

replaceEscapeSigns :: String -> String
replaceEscapeSigns = replace escapeSeq escapePlaceHolder

removeEscapePlaceholder :: String -> String
removeEscapePlaceholder = replace escapePlaceHolder " "

compensateLinebreaks :: String -> String
compensateLinebreaks s = concatMap compensate $ lines s
  where
    compensate line = line ++ "\n" ++ genLinebreaks line
    genLinebreaks line = concat $ replicate (count line escapePlaceHolder) "\n"

-- | Remove new line escapes and join escaped lines together on one line
--   to simplify parsing later on. Escapes are replaced with line breaks
--   to not alter the line numbers.
normalizeEscapedLines :: String -> String
normalizeEscapedLines s = removeEscapePlaceholder $ compensateLinebreaks $ replaceEscapeSigns $ trimLines s
