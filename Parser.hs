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
  f <- lexeme funcDef
  return $ CompUnit f

funcDef :: Parser FuncDef
funcDef = do
  ft <- lexeme1 funcType
  id <- lexeme ident
  lexeme $ char '('
  lexeme $ char ')'
  b <- block
  return $ FuncDef ft id b

funcType :: Parser FuncType
funcType = do
  _ <- string "int"
  return FInt

block :: Parser Block
block = do
  lexeme $ char '{'
  items <- many (lexeme blockItem)
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
    lexeme1 $ string "const"
    t <- lexeme1 btype
    ds <- sepBy1 (lexeme constDef) (lexeme $ char ',')
    char ';'
    return $ ConstDecl t ds)
  <|>
  (do
    t <- lexeme1 btype
    ds <- sepBy1 (lexeme varDef) (lexeme $ char ',')
    char ';'
    return $ VarDecl t ds)

btype :: Parser BType
btype = do
  string "int"
  return BInt

constDef :: Parser ConstDef
constDef = do
  id <- lexeme ident
  lexeme $ char '='
  a <- addExp
  return $ ConstDef id a

varDef :: Parser VarDef
varDef = try
  (do
    id <- lexeme ident
    lexeme $ char '='
    e <- expr
    return $ VarDef2 id e)
  <|>
  (do
    id <- ident
    return $ VarDef1 id)

stmt :: Parser Stmt
stmt = try
  (do
    lval <- lexeme ident
    lexeme $ char '='
    e <- lexeme expr
    char ';'
    return $ Stmt1 (LVal lval) e)
  <|> try
  (do
    e <- lexeme expr
    char ';'
    return $ Stmt2 e)
  <|> try
  (do
    char ';'
    return $ StmtSemiColon)
  <|> try
  (do
    lexeme $ string "return"
    e <- lexeme expr
    char ';'
    return $ StmtReturn e)
  <|> try
  (do
    b <- block
    return $ StmtBlock b)
  <|>
  (do
    lexeme $ string "if"
    lexeme $ char '('
    c <- lexeme cond
    lexeme $ char ')'
    s1 <- lexeme stmt
    s2 <- option StmtSemiColon (do
      string "else"
      lookAhead (noneOf ("_"++['a'..'z']++['A'..'Z']))
      many whitespace
      stmt)
    return $ StmtIfElse c s1 s2)

--------------------------------- Exp ------------------------------------------

expr :: Parser Exp
expr = addExp

addExp :: Parser AddExp
addExp = try
  (do
    m <- lexeme mulExp
    c <- lexeme (char '+' <|> char '-')
    a <- addExp
    if c == '+'
    then return $ AddExp2 m Add a
    else return $ AddExp2 m Sub a)
  <|>
  (do
    m <- mulExp
    return $ AddExp1 m)

mulExp :: Parser MulExp
mulExp = try
  (do
    u <- lexeme unaryExp
    c <- lexeme (char '*' <|> char '/' <|> char '%')
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
    id <- lexeme ident
    lexeme $ char '('
    es <- sepBy (lexeme expr) (lexeme $ char ',')
    char ')'
    return $ UnaryExpCallFunc id es)
  <|> try
  (do
    c <- lexeme (char '+' <|> char '-' <|> char '!')
    u <- unaryExp
    let op = case c of '+' -> Pos
                       '-' -> Neg
                       '!' -> LNot
     in return $ UnaryExp2 op u)
  <|>
  (do
    p <- primaryExp
    return $ UnaryExp1 p)

primaryExp :: Parser PrimaryExp
primaryExp = try
  (do
    lexeme $ char '('
    e <- lexeme expr
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

----------------------------- Cond ---------------------------------------------
cond :: Parser Cond
cond = lorExp

lorExp :: Parser LOrExp
lorExp = try
  (do
    a <- lexeme landExp
    lexeme $ string "||"
    o <- lorExp
    return $ LOrExp2 a o)
  <|>
  (do
    a <- landExp
    return $ LOrExp1 a)

landExp :: Parser LAndExp
landExp = try
  (do
    e <- lexeme eqExp
    lexeme $ string "&&"
    a <- landExp
    return $ LAndExp2 e a)
  <|>
  (do
    e <- eqExp
    return $ LAndExp1 e)

eqExp :: Parser EqExp
eqExp = try
  (do
    r <- lexeme relExp
    eq <- lexeme $ string "==" <|> string "!="
    e <- eqExp
    return $ EqExp2 r eq e)
  <|>
  (do
    r <- relExp
    return $ EqExp1 r)

relExp :: Parser RelExp
relExp = try
  (do
    a <- lexeme addExp
    rel <- lexeme $ try (string "<=") <|> try (string ">=") <|> string "<" <|> string ">"
    r <- relExp
    return $ RelExp2 a rel r)
  <|>
  (do
    a <- addExp
    return $ RelExp1 a)

lexeme :: Parser a -> Parser a
lexeme p = p <* many whitespace
lexeme1 :: Parser a -> Parser a
lexeme1 p = p <* many1 whitespace

whitespace :: Parser ()
whitespace = void space <|> blockComment <|> lineComment
  where
    blockComment = void $ try (string "/*")
                   *> manyTill anyChar (try $ string "*/")
    lineComment = void $ try (string "//")
                  *> manyTill anyChar (void eol <|> eof)

eol = do
  try (string "\r\n") <|> try (string "\n\r") <|> string "\n" <|> string "\r"

