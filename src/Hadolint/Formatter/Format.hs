module Hadolint.Formatter.Format
  ( severityText,
    stripNewlines,
    errorMessageLine,
    errorPosition,
    errorPositionPretty,
    Text.Megaparsec.Error.errorBundlePretty,
    Result (..),
    toResult,
  )
where

import Data.List (sort)
import qualified Data.List.NonEmpty as NE
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Hadolint.Rules
import Text.Megaparsec (TraversableStream (..), pstateSourcePos)
import Text.Megaparsec.Error
import Text.Megaparsec.Pos (SourcePos, sourcePosPretty)
import Text.Megaparsec.Stream (VisualStream)

data Result s e = Result
  { errors :: !(Seq (ParseErrorBundle s e)),
    checks :: !(Seq RuleCheck)
  }

instance Semigroup (Result s e) where
  (Result e1 c1) <> (Result e2 c2) = Result (e1 <> e2) (c1 <> c2)

instance Monoid (Result s e) where
  mappend = (<>)
  mempty = Result mempty mempty

toResult :: Either (ParseErrorBundle s e) [RuleCheck] -> Result s e
toResult res =
  case res of
    Left err -> Result (Seq.singleton err) mempty
    Right c -> Result mempty (Seq.fromList (sort c))

severityText :: DLSeverity -> String
severityText s =
  case s of
    DLErrorC -> "error"
    DLWarningC -> "warning"
    DLInfoC -> "info"
    DLStyleC -> "style"
    _ -> ""

stripNewlines :: String -> String
stripNewlines =
  map
    ( \c ->
        if c == '\n'
          then ' '
          else c
    )

errorMessageLine :: (VisualStream s, TraversableStream s, ShowErrorComponent e) => ParseErrorBundle s e -> String
errorMessageLine err@(ParseErrorBundle e _) =
  errorPositionPretty err ++ " " ++ parseErrorTextPretty (NE.head e)

errorPositionPretty :: TraversableStream s => ParseErrorBundle s e -> String
errorPositionPretty err = sourcePosPretty (errorPosition err)

errorPosition :: TraversableStream s => ParseErrorBundle s e -> Text.Megaparsec.Pos.SourcePos
errorPosition (ParseErrorBundle e s) =
  let (_, posState) = reachOffset (errorOffset (NE.head e)) s
   in pstateSourcePos posState
