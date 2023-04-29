module Main(main) where

import Parser
import Text.Parsec.Text(parseFromFile)
import Reader(reader)
import Simulation ( emptyState, program2list, allSteps )
import Interface (runUI)

main :: IO ()
main = do
  filePath <- reader
  result <- parseFromFile parseProgram filePath
  case result of
    Left err -> print err
    Right program -> do
      let
        funs = program2list program
        states = allSteps funs emptyState
      runUI states filePath
