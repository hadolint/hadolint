module Hadolint.Pragma
  ( ignored,
    parseShell
  )
  where

import Data.Functor.Identity (Identity)
import Data.Text (Text)
import Data.Void (Void)
import Hadolint.Rule (RuleCode (RuleCode))
import Language.Docker.Syntax
import qualified Control.Foldl as Foldl
import qualified Data.IntMap.Strict as Map
import qualified Data.Set as Set
import qualified Text.Megaparsec as Megaparsec
import qualified Text.Megaparsec.Char as Megaparsec


ignored :: Foldl.Fold (InstructionPos Text) (Map.IntMap (Set.Set RuleCode))
ignored = Foldl.Fold parse mempty id
  where
    parse acc InstructionPos {instruction = Comment comment, lineNumber = line} =
      case parseComment comment of
        Just ignores@(_ : _) -> Map.insert (line + 1) (Set.fromList . fmap RuleCode $ ignores) acc
        _ -> acc
    parse acc _ = acc

parseComment :: Text -> Maybe [Text]
parseComment =
  Megaparsec.parseMaybe commentParser

commentParser :: Megaparsec.Parsec Void Text [Text]
commentParser =
  do
    spaces
    >> string "hadolint"
    >> spaces1
    >> string "ignore="
    >> spaces
    >> Megaparsec.sepBy1 ruleName (spaces >> string "," >> spaces)

ruleName :: Megaparsec.ParsecT Void Text Identity (Megaparsec.Tokens Text)
ruleName = Megaparsec.takeWhile1P Nothing (\c -> c `elem` Set.fromList "DLSC0123456789")


parseShell :: Text -> Maybe Text
parseShell = Megaparsec.parseMaybe shellParser

shellParser :: Megaparsec.Parsec Void Text Text
shellParser =
  do
    spaces
    >> string "hadolint"
    >> spaces1
    >> string "shell"
    >> spaces
    >> string "="
    >> spaces
    >> shellName

shellName :: Megaparsec.ParsecT Void Text Identity (Megaparsec.Tokens Text)
shellName = Megaparsec.takeWhile1P Nothing (/= '\n')


string :: Megaparsec.Tokens Text
  -> Megaparsec.ParsecT Void Text Identity (Megaparsec.Tokens Text)
string = Megaparsec.string

spaces :: Megaparsec.ParsecT Void Text Identity (Megaparsec.Tokens Text)
spaces = Megaparsec.takeWhileP Nothing space

spaces1 :: Megaparsec.ParsecT Void Text Identity (Megaparsec.Tokens Text)
spaces1 = Megaparsec.takeWhile1P Nothing space

space :: Char -> Bool
space c = c == ' ' || c == '\t'
