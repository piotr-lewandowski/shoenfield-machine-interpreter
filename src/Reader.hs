{-# LANGUAGE BangPatterns #-}
{-# OPTIONS -Wall #-}
module Reader where

import System.IO
import System.FilePath ()
import Control.Exception (try)
import System.Directory.Internal.Prelude   

reader :: IO String
reader =  do
    putStr "Enter file path: "
    hFlush stdout
    filePath <- getLine
    print ("Opening file " ++ filePath)
    value <- tryIOError (readFile filePath)
    case value of
        Left err -> do
            print ("Error: Couldn't open file " ++ filePath ++ ". Try again.")
            reader
        Right content -> do
            print ("Successfully opened file " ++ filePath)
            return filePath