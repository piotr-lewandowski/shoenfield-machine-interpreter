module Main (main) where

import Parser
import Text.Parsec.Text(parseFromFile)

main :: IO ()
main = do
  filePath <- getLine
  result <- parseFromFile parseProgram filePath
  case result of
    Left err -> print err
    Right program -> print program
