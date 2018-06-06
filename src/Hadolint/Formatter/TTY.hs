{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Hadolint.Formatter.TTY
    ( printResult
    , formatError
    ) where

import qualified Data.List.NonEmpty as NE
import Data.Semigroup ((<>))
import qualified Data.Text as Text
import Hadolint.Formatter.Format
import Hadolint.Rules
import Language.Docker.Syntax
import Text.Megaparsec.Error
       (ParseError, ShowErrorComponent, ShowToken, errorPos,
        parseErrorTextPretty)
import Text.Megaparsec.Pos (sourcePosPretty)

formatErrors ::
       (ShowToken t, Ord t, ShowErrorComponent e, Functor f) => f (ParseError t e) -> f String
formatErrors = fmap formatError

formatError :: (ShowToken t, Ord t, ShowErrorComponent e) => ParseError t e -> String
formatError err = posPart ++ " " ++ stripNewlines (parseErrorTextPretty err)
  where
    pos = NE.head (errorPos err)
    posPart = sourcePosPretty pos

formatChecks :: Functor f => f RuleCheck -> f Text.Text
formatChecks = fmap formatCheck
  where
    formatCheck (RuleCheck meta source line _) =
        formatPos source line <> code meta <> " " <> message meta

formatPos :: Filename -> Linenumber -> Text.Text
formatPos source line = source <> ":" <> Text.pack (show line) <> " "

printResult :: (ShowToken t, Ord t, ShowErrorComponent e) => Result t e -> IO ()
printResult Result {errors, checks} = printErrors >> printChecks
  where
    printErrors = mapM_ putStrLn (formatErrors errors)
    printChecks = mapM_ (putStrLn . Text.unpack) (formatChecks checks)
