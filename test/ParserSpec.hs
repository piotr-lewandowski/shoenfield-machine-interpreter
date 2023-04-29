module ParserSpec (spec) where

import qualified Data.Text as T
import Instructions
import Parser
import Test.Hspec
import Test.QuickCheck
import Text.Parsec

instance Arbitrary Instruction where
  arbitrary = do
    register <- chooseInteger (0, 1000)
    instruction <- chooseInteger (0, 1000)
    elements [Inc register, Dec register instruction]

prop_roundtrip :: Program -> Bool
prop_roundtrip program = parse parseProgram "" programText == Right program
  where
    programText = T.intercalate (T.singleton ' ') $ map (T.pack . show) program

spec :: Spec
spec =
  describe "Program parser" $ do
    it "Roundtrips on random data" $ property prop_roundtrip