module Parser where

import Text.Parsec hiding (label)
import Text.Parsec.String (Parser)
import Text.Parsec.Combinator (sepBy)

import Data.ByteString.Char8 (pack)
import Data.String.Utils (replace, endswith, splitWs, strip)
import Control.Monad (void)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Token

import Lexer
import Syntax

comment :: Parser Instruction
comment = do
  char '#'
  text <- untilEol
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
  eol
  return $ Cmd args

copy :: Parser Instruction
copy = do
  reserved "COPY"
  src <- many (noneOf " ")
  Token.whiteSpace lexer
  dst <- many (noneOf "\n")
  eol
  return $ Copy src dst

stopsignal :: Parser Instruction
stopsignal = do
  reserved "STOPSIGNAL"
  args <- many (noneOf "\n")
  eol
  return $ Stopsignal args

quotedValue:: Parser String
quotedValue = do
  str <- stringLiteral
  return str

rawValue :: Parser String
rawValue = do
  str <- many (noneOf [' ','=','\n'])
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
  eol
  return $ Label p

env :: Parser Instruction
env = do
  reserved "ENV"
  p <- pairs
  eol
  return $ Env p

user :: Parser Instruction
user = do
  reserved "USER"
  args <- many (noneOf "\n")
  eol
  return $ User args

add :: Parser Instruction
add = do
  reserved "ADD"
  src <- many (noneOf " ")
  Token.whiteSpace lexer
  dst <- many (noneOf "\n")
  eol
  return $ Add src dst

expose :: Parser Instruction
expose = do
  reserved "EXPOSE"
  port <- natural
  eol
  return $ Expose port

run :: Parser Instruction
run = do
  reserved "RUN"
  cmd <- multilineArguments
  eol
  return $ Run cmd

-- Entire value until end of line, if line ends with escape character
-- the new line is consumed as well until a new line without escape character
-- is reached
multiline :: Parser String
multiline = do
  line <- many (noneOf "\n")
  eol
  if endswith "\\" line
    then do
        newLine <- multiline
        return $ line ++ newLine
    else return $ replace "\\" "" line

-- Parse value until end of line is reached
untilEol :: Parser String
untilEol = do
  line <- many (noneOf "\n")
  many eol
  return line

workdir :: Parser Instruction
workdir = do
  reserved "WORKDIR"
  directory <- many (noneOf "\n")
  eol
  return $ Workdir directory

volume :: Parser Instruction
volume = do
  reserved "VOLUME"
  directory <- many (noneOf "\n")
  eol
  return $ Volume directory

maintainer :: Parser Instruction
maintainer = do
  reserved "MAINTAINER"
  name <- untilEol
  return $ Maintainer name

-- Parse arguments of a command in the exec form
argumentsExec :: Parser Arguments
argumentsExec = do
  args <- brackets $ commaSep stringLiteral
  eol
  return $ args

-- Parse arguments of a command in the shell form
argumentsShell :: Parser Arguments
argumentsShell = do
  args <- sepBy rawValue (char ' ')
  eol
  return $ args

-- Parse arguments of a command in the shell form
multilineArgumentsShell :: Parser Arguments
multilineArgumentsShell = do
  line <- multiline
  return $ splitWs line

arguments :: Parser Arguments
arguments = try argumentsExec <|> try argumentsShell

multilineArguments :: Parser Arguments
multilineArguments = try argumentsExec <|> try multilineArgumentsShell

entrypoint :: Parser Instruction
entrypoint = do
  reserved "ENTRYPOINT"
  args <- arguments
  eol
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
    return $ parse (contents dockerfile) "<file>" program
