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
  lexemeA1 $ string "return"
  ret <- lexemeA number
  lexemeA $ char ';'
  lexemeA $ char '}'
  if 0 <= ret && ret <= 2147483647 then
    return $ Block (Return ret)
  else
    fail $ "return value "++show ret++" not in [0, 2147483647]!"

eol = do
  try (string "\r\n") <|> try (string "\n\r") <|> string "\n" <|> string "\r"

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
