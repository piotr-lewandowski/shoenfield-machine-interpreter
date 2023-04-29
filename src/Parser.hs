module Parser (parseProgram, parseArgs) where

import Instructions
import Text.Parsec
import Text.Parsec.Text (Parser)

parseInc :: Parser Instruction
parseInc = try $ do
  _ <- choice [try $ string "INC", try $ string "inc", try $ string "Inc"]
  spaces
  Inc <$> nonNegativeInt

parseDec :: Parser Instruction
parseDec = try $ do
  _ <- choice [try $ string "DEC", try $ string "dec", try $ string "Dec"]
  spaces
  register <- nonNegativeInt
  spaces
  Dec register <$> nonNegativeInt

parseInstruction :: Parser Instruction
parseInstruction = choice [parseInc, parseDec]

parseProgram :: Parser Program
parseProgram = sepBy parseInstruction spaces

nonNegativeInt :: (Num a, Read a) => Parser a
nonNegativeInt = try $ do
  x <- digit
  if x == '0'
    then pure 0
    else do
      xs <- many digit
      pure $ read (x : xs)

parseArgs :: (Num a, Read a) => Parser [a]
parseArgs = (0 :) <$> nonNegativeInt `sepBy` spaces
