module Lexer where

import Text.ParserCombinators.Parsec
import Numeric

number :: Parser Int
number = do
  x <- try decimalConst <|> try hexadecimalConst <|> octalConst
  if 0 <= x && x <= 2147483647
  then return x
  else error "number literal should be 0 <= x <= 2147483647"

decimalConst :: Parser Int
decimalConst = do
  firstDigit <- oneOf "123456789"
  remainingDigits <- many digit
  return $ read (firstDigit:remainingDigits)

octalConst :: Parser Int
octalConst = do
  firstDigit <- char '0'
  remainingDigits <- many octDigit
  return $ fst $ head $ readOct (firstDigit:remainingDigits)

hexadecimalConst :: Parser Int
hexadecimalConst = do
  prefix <- string "0x" <|> string "0X"
  digits <- many hexDigit
  return $ fst $ head $ readHex $ digits

ident :: Parser String
ident = do
  first <- nondigit
  remaining <- many (digit <|> nondigit)
  return (first:remaining)
    where nondigit = oneOf ("_"++['a'..'z']++['A'..'Z'])

