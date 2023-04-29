module Main (main) where

import Interface (runUI)
import Parser (parseArgs, parseProgram)
import Reader (reader)
import Simulation (allSteps, initialState, program2list)
import Text.Parsec.Text (parseFromFile)

main :: IO ()
main = do
  (programPath, argsPath) <- reader
  resultProgram <- parseFromFile parseProgram programPath
  resultArgs <- parseFromFile parseArgs argsPath
  let input = do
        program <- resultProgram
        args <- resultArgs
        pure (program, args)
  case input of
    Left err -> print err
    Right (program, args) -> do
      let funs = program2list program
          states = allSteps funs $ initialState args
      runUI states programPath
