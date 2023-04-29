module Reader (reader) where

import System.Directory.Internal.Prelude
import System.FilePath ()
import System.IO

reader :: IO (String, String)
reader = do
  program <- readPath "program"
  args <- readPath "arguments"
  pure (program, args)

readPath :: String -> IO String
readPath name = do
  putStr $ "Enter " ++ show name ++ " file path:"
  hFlush stdout
  filePath <- getLine
  print ("Opening file " ++ filePath)
  value <- tryIOError (readFile filePath)
  case value of
    Left _ -> do
      print ("Error: Couldn't open file " ++ filePath ++ ". Try again.")
      readPath name
    Right _ -> do
      print ("Successfully opened file " ++ filePath)
      return filePath