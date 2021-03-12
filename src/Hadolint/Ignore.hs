module Hadolint.Ignore (ignored) where

import qualified Control.Foldl as Foldl
import qualified Data.IntMap.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Void (Void)
import Hadolint.Rule (RuleCode (RuleCode))
import Language.Docker.Syntax
import qualified Text.Megaparsec as Megaparsec
import qualified Text.Megaparsec.Char as Megaparsec

ignored :: Foldl.Fold (InstructionPos Text.Text) (Map.IntMap (Set.Set RuleCode))
ignored = Foldl.Fold parse mempty id
  where
    parse acc InstructionPos {instruction = Comment comment, lineNumber = line} =
      case parseComment comment of
        Just ignores@(_ : _) -> Map.insert (line + 1) (Set.fromList . fmap RuleCode $ ignores) acc
        _ -> acc
    parse acc _ = acc

    parseComment :: Text.Text -> Maybe [Text.Text]
    parseComment =
      Megaparsec.parseMaybe commentParser

    commentParser :: Megaparsec.Parsec Void Text.Text [Text.Text]
    commentParser =
      do
        spaces
        >> string "hadolint"
        >> spaces1
        >> string "ignore="
        >> spaces
        >> Megaparsec.sepBy1 ruleName (spaces >> string "," >> spaces)

    ruleName = Megaparsec.takeWhile1P Nothing (\c -> c `elem` Set.fromList "DLSC0123456789")
    string = Megaparsec.string
    spaces = Megaparsec.takeWhileP Nothing space
    spaces1 = Megaparsec.takeWhile1P Nothing space
    space c = c == ' ' || c == '\t'
