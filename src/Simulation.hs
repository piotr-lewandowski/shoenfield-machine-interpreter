{-# LANGUAGE RecordWildCards #-}

module Simulation where

import Instructions

data State = State
  { register :: [Int],
    instructionCounter :: Int
  }

-- Show który wypisuje pierwszy rejestr gdzie znajduje się Wynik
instance Show State where
  show State {..} = show $ take 1 register

-- Alternatywny show którego używałem do testowania wypisuje n rejestrów. Przydatne przy obliczeniach krok po kroku
showState :: Int -> State -> String
showState n State {..} = "Rejestr: " ++ show (take n register) ++ " Licznik: " ++ show instructionCounter

-- Funkcja służy do generwoania stanu pustego
emptyState :: State
emptyState = State {register = repeat 0, instructionCounter = 0}

-- Funkcja służy do generwania stanu początkowego dla danych wejściowych w formie listy
initialState :: [Int] -> State
initialState x = State {register = x ++ repeat 0, instructionCounter = 0}

-- Funkcja INC
inc :: Int -> State -> State
inc i state@State {..} = state {register = updateList i (+ 1) register, instructionCounter = instructionCounter + 1}
  where
    updateList n f xs = take n xs ++ [f (xs !! n)] ++ drop (n + 1) xs

-- Funkcja DEC
dec :: Int -> Int -> State -> State
dec i n state@State {..}
  | register !! i == 0 = state {instructionCounter = instructionCounter + 1}
  | otherwise = state {register = updateList i (\x -> x - 1) register, instructionCounter = n}
  where
    updateList n f xs = take n xs ++ [f (xs !! n)] ++ drop (n + 1) xs

-- Funkcja zwraca wynik obliczeń programu (zapisanego w postaci listy funkcji inc oraz dec z podanymi parametrami) dla stanu początkowego
-- Przykładowe wywołanie: obliczenie 2+3
-- x = initialState [2,3]
-- obliczenia [inc 0,dec 0 3,inc 0,dec 1 2] x
runProgram :: [State -> State] -> State -> State
runProgram list state@State {..}
  | instructionCounter >= length list = state
  | otherwise = runProgram list ((list !! instructionCounter) state)

-- Funkcja ta to złożenie obliczenia oraz initialState - łatwiej się testuje
runProgram' :: [State -> State] -> [Int] -> State
runProgram' program wejscie = runProgram program (initialState wejscie)

-- Funkcja wykonuje jeden raz program (o ile to możliwe) dla danego stanu
runProgramStepByStep :: [State -> State] -> State -> State
runProgramStepByStep list state@State {..}
  | instructionCounter >= length list = state
  | otherwise = (list !! instructionCounter) state

program2list :: Program -> [State -> State]
program2list [] = []
program2list ((Inc x) : xs) = inc (fromIntegral x) : program2list xs
program2list ((Dec x y) : xs) = dec (fromIntegral x) (fromIntegral y) : program2list xs

allSteps :: [State -> State] -> State -> [State]
allSteps prog initState = initState : allSteps prog (runProgramStepByStep prog initState)
