-- Notice:
-- 1. Sequence matters
module Parser where

import Text.ParserCombinators.Parsec
import Control.Monad (void)

import Lexer
import Syntax

compUnit :: Parser CompUnit
compUnit = do
  many whitespace
  f <- lexemeA funcDef
  return $ CompUnit f

funcDef :: Parser FuncDef
funcDef = do
  ft <- lexemeA1 funcType
  id <- lexemeA ident
  lexemeA $ char '('
  lexemeA $ char ')'
  b <- block
  return $ FuncDef ft id b

funcType :: Parser FuncType
funcType = do
  _ <- string "int"
  return FInt

block :: Parser Block
block = do
  lexemeA $ char '{'
  items <- many (lexemeA blockItem)
  char '}'
  return $ Block items

blockItem :: Parser BlockItem
blockItem = try
  (do
    d <- decl
    return $ BlockItem1 d)
  <|>
  (do
    s <- stmt
    return $ BlockItem2 s)

decl :: Parser Decl
decl = try
  (do
    lexemeA1 $ string "const"
    t <- lexemeA1 btype
    ds <- sepBy1 (lexemeA constDef) (lexemeA $ char ',')
    char ';'
    return $ ConstDecl t ds)
  <|>
  (do
    t <- lexemeA1 btype
    ds <- sepBy1 (lexemeA varDef) (lexemeA $ char ',')
    char ';'
    return $ VarDecl t ds)

btype :: Parser BType
btype = do
  string "int"
  return BInt

constDef :: Parser ConstDef
constDef = do
  id <- lexemeA ident
  lexemeA $ char '='
  a <- addExp
  return $ ConstDef id a

varDef :: Parser VarDef
varDef = try
  (do
    id <- lexemeA ident
    lexemeA $ char '='
    e <- expr
    return $ VarDef2 id e)
  <|>
  (do
    id <- ident
    return $ VarDef1 id)

stmt :: Parser Stmt
stmt = try
  (do
    lval <- lexemeA ident
    lexemeA $ char '='
    e <- lexemeA expr
    char ';'
    return $ Stmt1 (LVal lval) e)
  <|> try
  (do
    e <- lexemeA expr
    char ';'
    return $ Stmt2 e)
  <|> try
  (do
    char ';'
    return $ SemiColon)
  <|>
  (do
    lexemeA $ string "return"
    e <- lexemeA expr
    char ';'
    return $ Return e)

expr :: Parser Exp
expr = addExp

addExp :: Parser AddExp
addExp = try
  (do
    m <- lexemeA mulExp
    c <- lexemeA (char '+' <|> char '-')
    a <- addExp
    if c == '+'
    then return $ AddExp2 m Pos a
    else return $ AddExp2 m Neg a)
  <|>
  (do
    m <- mulExp
    return $ AddExp1 m)

mulExp :: Parser MulExp
mulExp = try
  (do
    u <- lexemeA unaryExp
    c <- lexemeA (char '*' <|> char '/' <|> char '%')
    m <- mulExp
    let op = case c of '*' -> Mul
                       '/' -> Div
                       '%' -> Mod
     in return $ MulExp2 u op m)
  <|>
  (do
    u <- unaryExp
    return $ MulExp1 u)

unaryExp :: Parser UnaryExp
unaryExp = try
  (do
    id <- lexemeA ident
    lexemeA $ char '('
    es <- sepBy (lexemeA expr) (lexemeA $ char ',')
    char ')'
    return $ UnaryExp3 id es)
  <|> try
  (do
    c <- lexemeA (char '+' <|> char '-')
    u <- unaryExp
    let op = case c of '+' -> Pos
                       '-' -> Neg
     in return $ UnaryExp2 op u)
  <|>
  (do
    p <- primaryExp
    return $ UnaryExp1 p)

primaryExp :: Parser PrimaryExp
primaryExp = try
  (do
    lexemeA $ char '('
    e <- lexemeA expr
    char ')'
    return $ PrimaryExp1 e)
  <|> try
  (do
    n <- number
    return $ PrimaryExp2 n)
  <|>
  (do
    lval <- ident
    return $ PrimaryExp3 (LVal lval))

lexemeA :: Parser a -> Parser a
lexemeA p = p <* many whitespace
lexemeA1 :: Parser a -> Parser a
lexemeA1 p = p <* many1 whitespace

whitespace :: Parser ()
whitespace = void space <|> blockComment <|> lineComment
  where
    blockComment = void $ try (string "/*")
                   *> manyTill anyChar (try $ string "*/")
    lineComment = void $ try (string "//")
                  *> manyTill anyChar (void eol <|> eof)

eol = do
  try (string "\r\n") <|> try (string "\n\r") <|> string "\n" <|> string "\r"

