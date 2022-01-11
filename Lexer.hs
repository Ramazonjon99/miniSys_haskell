module Lexer where

import Text.ParserCombinators.Parsec
import Numeric

number::Parser Int
number = try decimalConst <|> try hexadecimalConst <|> octalConst

decimalConst::Parser Int
decimalConst = do
  firstDigit <- oneOf "123456789"
  remainingDigits <- many digit
  return $ read (firstDigit:remainingDigits)

octalConst::Parser Int
octalConst = do
  firstDigit <- char '0'
  remainingDigits <- many octDigit
  return $ fst $ head $ readOct (firstDigit:remainingDigits)

hexadecimalConst::Parser Int
hexadecimalConst = do
  prefix <- string "0x" <|> string "0X"
  digits <- many hexDigit
  return $ fst $ head $ readHex $ digits

