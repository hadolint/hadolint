module Hadolint.Parser where

import Text.Parsec hiding (label)
import Text.Parsec.String (Parser)
import Text.Parsec.Combinator (sepBy)
import Text.Parsec.Pos (sourceLine)

import Data.ByteString.Char8 (pack)
import Control.Monad (void)
import Data.List.Split (splitOn)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Token

import Debug.Trace
import Hadolint.Lexer
import Hadolint.Syntax
import Hadolint.Normalize

comment :: Parser Instruction
comment = do
  char '#'
  text <- untilEol
  return $ Comment text

taggedImage :: Parser BaseImage
taggedImage = do
  name <- untilOccurrence ":\n"
  oneOf ":"
  tag <- untilEol
  return $ TaggedImage name tag

digestedImage :: Parser BaseImage
digestedImage = do
  name <- untilOccurrence "@\n"
  oneOf "@"
  digest <- untilEol
  return $ DigestedImage name (pack digest)

untaggedImage :: Parser BaseImage
untaggedImage = do
  name <- many (noneOf "\n")
  return $ UntaggedImage name

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

shell :: Parser Instruction
shell = do
  reserved "SHELL"
  args <- arguments
  return $ Shell args

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

-- We cannot use string literal because it swallows space
-- and therefore have to implement quoted values by ourselves
quotedValue:: Parser String
quotedValue = do
    char '"'
    literal <- untilOccurrence "\""
    char '"'
    return literal

rawValue :: Parser String
rawValue = many1 (noneOf [' ','=','\n'])

singleValue :: Parser String
singleValue = try quotedValue <|> try rawValue

pair :: Parser (String, String)
pair = do
  key <- rawValue
  oneOf "= "
  value <- singleValue
  return (key, value)

pairs :: Parser Pairs
pairs = do
    _ <- many (char ' ')
    first <- pair
    next <- remainingPairs
    return (first:next)

remainingPairs =
    try (char ' ' >> pairs)
    <|> try (return [])

label :: Parser Instruction
label = do
  reserved "LABEL"
  p <- pairs
  return $ Label p

arg :: Parser Instruction
arg = do
  reserved "ARG"
  p <- untilEol
  return $ Arg p

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
  src <- untilOccurrence " "
  Token.whiteSpace lexer
  dst <- untilOccurrence "\n"
  return $ Add src dst

expose :: Parser Instruction
expose = do
  reserved "EXPOSE"
  ports <- many natural
  return $ Expose ports

run :: Parser Instruction
run = do
  reserved "RUN"
  cmd <- arguments
  return $ Run cmd

-- Parse value until end of line is reached
untilEol :: Parser String
untilEol = many (noneOf "\n")

untilOccurrence :: String -> Parser String
untilOccurrence t = many $ noneOf t

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
argumentsExec = brackets $ commaSep stringLiteral

-- Parse arguments of a command in the shell form
argumentsShell :: Parser Arguments
argumentsShell = do
    args <- untilEol
    return $ words args

arguments :: Parser Arguments
arguments = try argumentsExec <|> try argumentsShell

entrypoint :: Parser Instruction
entrypoint = do
  reserved "ENTRYPOINT"
  args <- arguments
  return $ Entrypoint args

onbuild :: Parser Instruction
onbuild = do
  reserved "ONBUILD"
  i <- parseInstruction
  return $ OnBuild i

healthcheck :: Parser Instruction
healthcheck = do
  reserved "HEALTHCHECK"
  args <- untilEol
  return $ Healthcheck args

parseInstruction :: Parser Instruction
parseInstruction
    = try onbuild
    <|> try from
    <|> try copy
    <|> try run
    <|> try workdir
    <|> try entrypoint
    <|> try volume
    <|> try expose
    <|> try env
    <|> try arg
    <|> try user
    <|> try label
    <|> try stopsignal
    <|> try cmd
    <|> try shell
    <|> try maintainer
    <|> try add
    <|> try comment
    <|> try healthcheck

contents :: Parser a -> Parser a
contents p = do
    Token.whiteSpace lexer
    r <- p
    eof
    return r

eol :: Parser ()
eol = void $ char '\n' <|> (char '\r' >> option '\n' (char '\n'))

dockerfile :: Parser Dockerfile
dockerfile = many $ do
    -- deal with empty lines that only contain spaces or tabs
    skipMany space
    skipMany $ char '\t'
    pos <- getPosition
    i <- parseInstruction
    many eol
    return $ InstructionPos i (sourceName pos) (sourceLine pos)

parseString :: String -> Either ParseError Dockerfile
parseString s = parse (contents dockerfile) "<string>" $ normalizeEscapedLines s

parseFile :: String -> IO (Either ParseError Dockerfile)
parseFile file = do
    program <- readFile file
    return $ parse (contents dockerfile) file $ normalizeEscapedLines program
