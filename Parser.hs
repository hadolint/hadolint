module Parser where

import Text.Parsec hiding (label)
import Text.Parsec.String (Parser)
import Text.Parsec.Combinator (sepBy)

import Data.ByteString.Char8 (pack)
import Control.Monad (void)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Token

import Lexer
import Syntax

comment :: Parser Instruction
comment = do
  char '#'
  text <- many (noneOf "\n")
  many eol
  return $ Comment text

taggedImage :: Parser BaseImage
taggedImage = do
  name <- many (noneOf ":")
  reservedOp ":"
  tag <- many (noneOf "\n")
  return $ TaggedImage name tag

digestedImage :: Parser BaseImage
digestedImage = do
  name <- many (noneOf "@")
  reservedOp "@"
  digest <- many (noneOf "\n")
  return $ DigestedImage name (pack digest)

untaggedImage :: Parser BaseImage
untaggedImage = do
  name <- many (noneOf "\n")
  return $ LatestImage name

baseImage :: Parser BaseImage
baseImage = try taggedImage
    <|> try digestedImage
    <|> try untaggedImage

from :: Parser Instruction
from = do
  reserved "FROM"
  image <- baseImage
  many eol
  return $ From image

cmd :: Parser Instruction
cmd = do
  reserved "CMD"
  args <- arguments
  many eol
  return $ Cmd args

copy :: Parser Instruction
copy = do
  reserved "COPY"
  src <- many (noneOf " ")
  Token.whiteSpace lexer
  dst <- many (noneOf "\n")
  many eol
  return $ Copy src dst

stopsignal :: Parser Instruction
stopsignal = do
  reserved "STOPSIGNAL"
  args <- many (noneOf "\n")
  many eol
  return $ Stopsignal args

quotedValue:: Parser String
quotedValue = do
  str <- stringLiteral
  return str

rawValue :: Parser String
rawValue = do
  str <- many (noneOf " =\n")
  return str

singleValue :: Parser String
singleValue = try quotedValue <|> try rawValue

pair :: Parser (String, String)
pair = do
  key <- singleValue
  oneOf "="
  value <- singleValue
  return (key, value)

pairs :: Parser Pairs
pairs = do
  p <- sepBy pair (char ' ')
  return p

label :: Parser Instruction
label = do
  reserved "LABEL"
  p <- pairs
  many eol
  return $ Label p

env :: Parser Instruction
env = do
  reserved "ENV"
  p <- pairs
  many eol
  return $ Env p

user :: Parser Instruction
user = do
  reserved "USER"
  args <- many (noneOf "\n")
  many eol
  return $ User args

add :: Parser Instruction
add = do
  reserved "ADD"
  src <- many (noneOf " ")
  Token.whiteSpace lexer
  dst <- many (noneOf "\n")
  many eol
  return $ Add src dst

expose :: Parser Instruction
expose = do
  reserved "EXPOSE"
  port <- natural
  many eol
  return $ Expose port

run :: Parser Instruction
run = do
  reserved "RUN"
  cmd <- arguments
  many eol
  return $ Run cmd

workdir :: Parser Instruction
workdir = do
  reserved "WORKDIR"
  directory <- many (noneOf "\n")
  many eol
  return $ Workdir directory

volume :: Parser Instruction
volume = do
  reserved "VOLUME"
  directory <- many (noneOf "\n")
  many eol
  return $ Volume directory

maintainer :: Parser Instruction
maintainer = do
  reserved "MAINTAINER"
  name <- many (noneOf "\n")
  many eol
  return $ Maintainer name

-- Parse arguments of a command in the exec form
argumentsExec :: Parser Arguments
argumentsExec = do
  args <- brackets $ commaSep stringLiteral
  return $ args

-- A value that can span over multiple lines
rawLongValue :: Parser Arguments
rawLongValue = do
  multiLines <- endBy (many (noneOf "\\\n")) (oneOf "\\\n")
  return $ concat multiLines

-- Parse arguments of a command in the shell form
argumentsShell :: Parser Arguments
argumentsShell = do
  args <- rawLongValue
  return $ args

arguments :: Parser Arguments
arguments = try argumentsExec <|> try argumentsShell

entrypoint :: Parser Instruction
entrypoint = do
  reserved "ENTRYPOINT"
  args <- arguments
  many eol
  return $ Entrypoint args

instruction :: Parser Instruction
instruction
    = try from
    <|> try copy
    <|> try run
    <|> try workdir
    <|> try entrypoint
    <|> try volume
    <|> try expose
    <|> try env
    <|> try user
    <|> try label
    <|> try stopsignal
    <|> try cmd
    <|> try maintainer
    <|> try add
    <|> try comment

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
