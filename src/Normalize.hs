module Normalize (
 normalizeEscapedLines
) where

import Data.List (intercalate)
import Data.List.Split (splitOn)
import Debug.Trace

replace old new = intercalate new . splitOn old

-- | Remove new line escapes and join escaped lines together on one line
--   to simplify parsing later on. Escapes are replaced with line breaks
--   to not alter the line numbers.
normalizeEscapedLines :: String -> String
normalizeEscapedLines s = unwords escapedSegments ++ lineBreakReplacements
  where
    escapedSegments = splitOn escapeSequence s
    lineBreakReplacements = concat $ replicate escapeOccurrences "\n"
    -- count gaps between elements
    escapeOccurrences = length escapedSegments - 1
    escapeSequence = "\\\n"
