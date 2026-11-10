module Hadolint.Rule.DL3003 (rule) where

import Hadolint.Rule
import Hadolint.Shell (ParsedShell (..))
import Language.Docker.Syntax (Instruction (..), RunArgs (..))
import qualified ShellCheck.AST as AST
import qualified ShellCheck.ASTLib as ASTLib
import ShellCheck.Interface (ParseResult (..))

rule :: Rule ParsedShell
rule = simpleRule code severity message check
  where
    code = "DL3003"
    severity = DLWarningC
    message = "Use WORKDIR to switch to a directory"
    check (Run (RunArgs args _)) = foldArguments (not . hasUnscopedCd) args
    check _ = True

    hasUnscopedCd = any go . prRoot . parsed
    -- An explicit subshell keeps directory changes local to that scope.
    go AST.T_Subshell {} = False
    go token@(AST.OuterToken _ children) =
      ASTLib.getCommandName token == Just "cd" || any go children
{-# INLINEABLE rule #-}
