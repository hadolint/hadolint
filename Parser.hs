module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)

import Data.ByteString.Char8 (pack)
import Control.Monad (void)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Token

import Lexer
import Syntax

ludi = TaggedImage "debian" "jessie"

arguments :: Parser String
arguments =
  many (noneOf ",\n")

taggedImage :: Parser BaseImage
taggedImage = do
  name <- many (noneOf ":")
  reservedOp ":"
  tag <- many (noneOf "\n")
  eol
  return $ TaggedImage name tag

digestedImage :: Parser BaseImage
digestedImage = do
  name <- many (noneOf "@")
  reservedOp "@"
  digest <- many (noneOf "\n")
  eol
  return $ DigestedImage name (pack digest)

untaggedImage :: Parser BaseImage
untaggedImage = do
  name <- many (noneOf "\n")
  eol
  return $ LatestImage name

baseImage :: Parser BaseImage
baseImage = try taggedImage
    <|> try digestedImage
    <|> try untaggedImage

from :: Parser Instruction
from = do
  reserved "FROM"
  image <- baseImage
  eol
  return $ From image

env :: Parser Instruction
env = do
  reserved "ENV"
  key <- many (noneOf [' ','='])
  _ <- oneOf[' ','=']
  value <- many (noneOf "\n")
  eol
  return $ Env key value

copy :: Parser Instruction
copy = do
  reserved "COPY"
  args <- arguments
  eol
  return $ Copy args

expose :: Parser Instruction
expose = do
  reserved "EXPOSE"
  port <- natural
  eol
  return $ Expose port

maintainer :: Parser Instruction
maintainer = do
  reserved "MAINTAINER"
  name <- many (noneOf "\n")
  eol
  return $ Maintainer name

instruction :: Parser Instruction
instruction = try from
    <|> try copy
    <|> try expose
    <|> try env
    <|> try maintainer

contents :: Parser a -> Parser a
contents p = do
    Token.whiteSpace lexer
    r <- p
    eof
    return r

eol :: Parser ()
eol = void (char '\n') <|> eof

dockerfile :: Parser Dockerfile
dockerfile = many $ do
    i <- instruction
    return i

parseString :: String -> Either ParseError Dockerfile
parseString input = parse (contents dockerfile) "<string>" input

parseFile :: String -> IO (Either ParseError Dockerfile)
parseFile file = do
    program <- readFile file
    return (parse (contents dockerfile) "<file>" program)
