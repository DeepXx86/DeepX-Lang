module Lexer where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Token

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser style
  where
    style = emptyDef
      { Token.commentLine = "//"
      , Token.reservedNames = ["if", "else", "while", "for", "func", "var", "import", "write", "No cap", "cap", "wait", "getLine"]
      , Token.reservedOpNames = ["+", "-", "*", "/", "=", "<", ">", "==", "=>"]
      }




integer :: Parser Int
integer = fromIntegral <$> Token.integer lexer

stringLiteral :: Parser String
stringLiteral = Token.stringLiteral lexer

identifier :: Parser String
identifier = Token.identifier lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

braces :: Parser a -> Parser a
braces = Token.braces lexer

comma :: Parser String
comma = Token.comma lexer
