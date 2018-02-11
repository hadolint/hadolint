module Hadolint.Formatter.Format
    ( formatErrorReason
    , severityText
    ) where

import Hadolint.Rules
import Language.Docker.Syntax
import ShellCheck.Interface
import Text.Parsec.Error
       (ParseError, errorMessages, errorPos, showErrorMessages)

severityText :: Severity -> String
severityText severity =
    case severity of
        ErrorC -> "error"
        WarningC -> "warning"
        InfoC -> "info"
        StyleC -> "style"

formatErrorReason :: ParseError -> String
formatErrorReason err = msgPart
  where
    msgPart =
        showErrorMessages
            "or"
            "unknown parse error"
            "expecting"
            "unexpected"
            "end of input"
            (errorMessages err)
    stripNewlines =
        map
            (\c ->
                 if c == '\n'
                     then ' '
                     else c)
