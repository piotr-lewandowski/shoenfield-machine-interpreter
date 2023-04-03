module Main (main) where

import Parser
import Text.Parsec.Text(parseFromFile)
import Reader(reader)

main :: IO ()
main = do
  filePath <- reader
  result <- parseFromFile parseProgram filePath
  case result of
    Left err -> print err
    Right program -> print program
