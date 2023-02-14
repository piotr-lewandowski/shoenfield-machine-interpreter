module Parser(parseProgram) where

import Instructions
import Text.Parsec
import Text.Parsec.Text(Parser)


parseInc :: Parser Instruction
parseInc = try $ do
    _ <- choice [try $ string "INC", try $ string "inc", try $ string "Inc"]
    spaces
    Inc <$> positiveInt

parseDec :: Parser Instruction
parseDec = try $ do
    _ <- choice [try $ string "DEC", try $ string "dec", try $ string "Dec"]
    spaces
    register <- positiveInt
    spaces
    Dec register <$> positiveInt

parseInstruction :: Parser Instruction
parseInstruction = choice [parseInc, parseDec]

parseProgram :: Parser Program
parseProgram = sepBy parseInstruction spaces

positiveInt :: (Read a) => Parser a
positiveInt = try $ do
    x <- oneOf "123456789"
    xs <- many digit
    return $ read (x:xs)