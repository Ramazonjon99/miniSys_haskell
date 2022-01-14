module Parser where

import Text.ParserCombinators.Parsec
import Control.Monad (void)

import Lexer
import Syntax

compUnit :: Parser CompUnit
compUnit = do
  _ <- many whitespace
  f <- lexemeA funcDef
  return $ CompUnit f

funcDef :: Parser FuncDef
funcDef = do
  ft <- funcType
  id <- ident
  lexemeA $ char '('
  lexemeA $ char ')'
  b <- block
  return $ FuncDef ft id b

funcType :: Parser FuncType
funcType = do
  _ <- lexemeA1 $ string "int"
  return TypeInt

ident :: Parser Ident
ident = do
  _ <- lexemeA $ string "main"
  return Main

block :: Parser Block
block = do
  lexemeA $ char '{'
  string "return"
  lookAhead (void $ char '(') <|> void (many1 whitespace)
  ret <- lexemeA expr
  lexemeA $ char ';'
  lexemeA $ char '}'
  return $ Block (Return ret)

expr :: Parser Exp
expr = do
  a <- addExp
  return $ Exp a

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
  <|>
  (do
    n <- number
    return $ PrimaryExp2 n)

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

