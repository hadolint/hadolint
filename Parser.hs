module Parser where

import Text.Parsec hiding (label)
import Text.Parsec.String (Parser)
import Text.Parsec.Combinator (sepBy)
import Text.Parsec.Pos (sourceLine)

import Data.ByteString.Char8 (pack)
import Control.Monad (void)
import Data.List.Split (splitOn)

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
  return $ From image

cmd :: Parser Instruction
cmd = do
  reserved "CMD"
  args <- arguments
  return $ Cmd args

copy :: Parser Instruction
copy = do
  reserved "COPY"
  src <- many (noneOf " ")
  Token.whiteSpace lexer
  dst <- many (noneOf "\n")
  return $ Copy src dst

stopsignal :: Parser Instruction
stopsignal = do
  reserved "STOPSIGNAL"
  args <- many (noneOf "\n")
  return $ Stopsignal args

quotedValue:: Parser String
quotedValue = stringLiteral

rawValue :: Parser String
rawValue = many (noneOf [' ','=','\n'])

singleValue :: Parser String
singleValue = try quotedValue <|> try rawValue

pair :: Parser (String, String)
pair = do
  key <- singleValue
  oneOf "="
  value <- singleValue
  return (key, value)

pairs :: Parser Pairs
pairs = sepBy pair (char ' ')

label :: Parser Instruction
label = do
  reserved "LABEL"
  p <- pairs
  return $ Label p

env :: Parser Instruction
env = do
  reserved "ENV"
  p <- pairs
  return $ Env p

user :: Parser Instruction
user = do
  reserved "USER"
  username <- untilEol
  return $ User username

add :: Parser Instruction
add = do
  reserved "ADD"
  src <- many (noneOf " ")
  Token.whiteSpace lexer
  dst <- many (noneOf "\n")
  return $ Add src dst

expose :: Parser Instruction
expose = do
  reserved "EXPOSE"
  port <- natural
  return $ Expose port

run :: Parser Instruction
run = do
  reserved "RUN"
  cmd <- multilineArguments
  return $ Run cmd

-- Entire value until end of line, if line ends with escape character
-- the new line is consumed as well until a new line without escape character
-- is reached
multiline :: Parser String
multiline = do
  line <- many (noneOf "\n")
  eol
  if head (reverse line) == '\\'
    then do
        newLine <- multiline
        return $ line ++ newLine
    else return $ line

-- Parse value until end of line is reached
untilEol :: Parser String
untilEol = do
  line <- many (noneOf "\n")
  return line

workdir :: Parser Instruction
workdir = do
  reserved "WORKDIR"
  directory <- many (noneOf "\n")
  return $ Workdir directory

volume :: Parser Instruction
volume = do
  reserved "VOLUME"
  directory <- many (noneOf "\n")
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
  return args

-- Parse arguments of a command in the shell form
argumentsShell :: Parser Arguments
argumentsShell = do
  args <- sepBy rawValue (char ' ')
  return args

-- Parse arguments of a command in the shell form
multilineArgumentsShell :: Parser Arguments
multilineArgumentsShell = do
  line <- multiline
  return $ splitOn " " line

arguments :: Parser Arguments
arguments = try argumentsExec <|> try argumentsShell

multilineArguments :: Parser Arguments
multilineArguments = try argumentsExec <|> try multilineArgumentsShell

entrypoint :: Parser Instruction
entrypoint = do
  reserved "ENTRYPOINT"
  args <- arguments
  return $ Entrypoint args

parseInstruction :: Parser Instruction
parseInstruction
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
eol = (char '\n' <|> (char '\r' >> option '\n' (char '\n'))) >> return ()

dockerfile :: Parser Dockerfile
dockerfile = many $ do
    pos <- getPosition
    i <- parseInstruction
    many eol
    return $ InstructionPos i $ sourceLine pos

parseString :: String -> Either ParseError Dockerfile
parseString input = parse (contents dockerfile) "<string>" input

parseFile :: String -> IO (Either ParseError Dockerfile)
parseFile file = do
    program <- readFile file
    return $ parse (contents dockerfile) file program
