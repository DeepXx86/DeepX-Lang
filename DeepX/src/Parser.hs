module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Expr as E

import AST
import Lexer


expr :: Parser Expr
expr = E.buildExpressionParser table term
  where
    table = [ [binary "*" Mul E.AssocLeft, binary "/" Div E.AssocLeft]
            , [binary "+" Add E.AssocLeft, binary "-" Sub E.AssocLeft]
            , [binary "<" LessThan E.AssocNone, binary ">" GreaterThan E.AssocNone]
            , [binary "==" Equals E.AssocNone]
            ]
    binary name f assoc = E.Infix (reservedOp name >> return f) assoc

ifExpr :: Parser Expr
ifExpr = do
    reserved "if"
    condition <- parens expr
    ifBody <- braces $ many expr
    elseBody <- optionMaybe (reserved "else" >> braces (many expr))
    return $ If condition ifBody elseBody


term :: Parser Expr
term =  try funcCallExpr
    <|> try assignExpr
    <|> try ifExpr       
    <|> try whileExpr
    <|> try forExpr
    <|> try waitExpr
    <|> try writeExpr  
    <|> try importExpr
    <|> try getLineExpr 
    <|> literalExpr
    <|> varExpr
    <|> parens expr


getLineExpr :: Parser Expr
getLineExpr = do
    reserved "getLine"
    prompt <- parens expr
    return $ GetLine prompt

literalExpr :: Parser Expr
literalExpr = Literal . VInt <$> integer
          <|> Literal . VString <$> stringLiteral

varExpr :: Parser Expr
varExpr = Var <$> identifier

assignExpr :: Parser Expr
assignExpr = do
    var <- identifier
    reservedOp "="
    Assign var <$> expr

funcCallExpr :: Parser Expr
funcCallExpr = do
    name <- identifier
    args <- parens $ expr `sepBy` comma
    return $ FuncCall name args

writeExpr :: Parser Expr
writeExpr = do
    reserved "write"
    Write <$> parens (expr `sepBy` comma)

importExpr :: Parser Expr
importExpr = do
    reserved "import"
    Import <$> stringLiteral



whileExpr :: Parser Expr
whileExpr = do
    reserved "while"
    condition <- parens expr
    body <- braces $ many expr
    return $ While condition body

forExpr :: Parser Expr
forExpr = do
    reserved "for"
    var <- identifier
    reservedOp "="
    reservedOp "("
    startExpr <- expr
    reservedOp ","
    endExpr <- expr
    reservedOp ")"
    body <- braces $ many expr
    return $ For var startExpr endExpr body

waitExpr :: Parser Expr
waitExpr = do
    reserved "wait"
    Wait <$> parens expr

funcDef :: Parser Expr
funcDef = do
    reserved "func"
    name <- identifier
    params <- parens $ identifier `sepBy` comma
    body <- braces $ many expr
    return $ FuncDef name params body

program :: Parser [Expr]
program = many (funcDef <|> expr)
