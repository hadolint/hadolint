module Hadolint.Rule.DL3059 (rule) where

import Hadolint.Rule
import qualified Hadolint.Shell as Shell
import Language.Docker.Syntax


data Acc
  = Acc { flags :: RunFlags, count :: Int }
  | Empty
  deriving (Eq)

-- | This Rule catches multiple consecutive `RUN` instructions.
-- It ignores the case where multiple commands are chained together (e.g. with
-- `&&`) because in that case the programmer most likely has deliberately
-- chosen to use multiple `RUN` instructions. Cases where --mount=xxx flags
-- differ are excluded as well.
rule :: Rule Shell.ParsedShell
rule = customRule check (emptyState Empty)
  where
    code = "DL3059"
    severity = DLInfoC
    message = "Multiple consecutive `RUN` instructions. Consider consolidation."

    check line st (Run (RunArgs ar fl))
      | state st == Empty =
          st |> modify (remember fl (foldArguments countCommands ar))
      | flags (state st) /= fl =
          st |> modify (remember fl (foldArguments countCommands ar))
      | foldArguments countCommands ar > 2 || count (state st) > 2 =
          st |> modify (remember fl (foldArguments countCommands ar))
      | otherwise = st |> addFail CheckFailure {..}
    check _ st (Comment _) = st
    check _ st _ = st |> modify reset
{-# INLINEABLE rule #-}

remember :: RunFlags -> Int -> Acc -> Acc
remember fl cn _ = Acc { flags = fl, count = cn }

reset :: Acc -> Acc
reset _ = Empty

countCommands :: Shell.ParsedShell -> Int
countCommands script = length $ Shell.presentCommands script
