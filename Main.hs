module Main where

import System.Environment (getArgs)
import Text.ParserCombinators.Parsec
import Parser
import Syntax (CompUnit)
import Emit (emit)

compile :: String -> String -> Either String String
compile inputFile input = case parse compUnit inputFile input of
                            (Left l) -> Left (show l)
                            (Right r) -> Right $ emit r

interactWith function inputFile outputFile = do
  input <- readFile inputFile
  case (function inputFile input) of (Right r) -> writeFile outputFile r
                                     (Left l) -> error l

main = mainWith compile
  where mainWith function = do
          args <- getArgs
          case args of
            [inputFile,outputFile] -> interactWith function inputFile outputFile
            _ -> error "error: exactly two arguments needed"
