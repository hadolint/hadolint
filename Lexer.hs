module Lexer where

import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Token

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser style
  where
    names = ["FROM","ADD","RUN","WORKDIR","EXPOSE","VOLUME","ENTRYPOINT","MAINTAINER","ENV"]
    ops = [":", "@"]
    style = emptyDef {
               Token.commentLine = "#"
             , Token.reservedNames = names
             , Token.reservedOpNames = ops
             }

reserved :: String -> Parser ()
reserved = Token.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

integer :: Parser Integer
integer = Token.integer lexer
