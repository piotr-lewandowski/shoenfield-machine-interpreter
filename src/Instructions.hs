module Instructions(Instruction(..), Program) where

type RegisterIndex = Integer

type InstructionIndex = Integer

type Program = [Instruction]

data Instruction = Inc RegisterIndex | Dec RegisterIndex InstructionIndex deriving (Show, Eq)