module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)

import Data.ByteString.Char8 (pack)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Token

import Lexer
import Syntax

eol = '\n'

ludi = TaggedImage "debian" "jessie"

arguments :: Parser String
arguments =
  many (noneOf ",\n")

taggedImage :: Parser BaseImage
taggedImage = do
  name <- many (noneOf ":")
  reservedOp ":"
  tag <- many (noneOf [eol])
  return $ TaggedImage name tag

digestedImage :: Parser BaseImage
digestedImage = do
  name <- many (noneOf "@")
  reservedOp "@"
  digest <- many (noneOf [eol])
  return $ DigestedImage name (pack digest)

untaggedImage :: Parser BaseImage
untaggedImage = do
  name <- many (noneOf [eol])
  return $ LatestImage name

baseImage :: Parser BaseImage
baseImage = try taggedImage
    <|> try digestedImage
    <|> try untaggedImage

from :: Parser Instruction
from = do
  reserved "FROM"
  image <- baseImage
  return $ From image

env :: Parser Instruction
env = do
  reserved "ENV"
  key <- many (noneOf [' ','='])
  value <- many (noneOf [eol])
  return $ Env key value

copy :: Parser Instruction
copy = do
  reserved "COPY"
  args <- arguments
  return $ Copy args

expose :: Parser Instruction
expose = do
  reserved "EXPOSE"
  port <- integer
  return $ Expose port

maintainer :: Parser Instruction
maintainer = do
  reserved "MAINTAINER"
  name <- many (noneOf [eol])
  return $ Maintainer name

instruction :: Parser Instruction
instruction = try from
    <|> try copy
    <|> try expose
    <|> try env
    <|> try maintainer

dockerfile :: Parser [Instruction]
dockerfile = do
    result <- many instruction
    eof
    return result

parseDockerfile:: String -> Either ParseError [Instruction]
parseDockerfile input = parse dockerfile "<stdin>" input
