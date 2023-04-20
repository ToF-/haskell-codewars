module SquareDigitSpec where
import SquareDigit
import Test.QuickCheck
import Test.Hspec

spec :: Spec
spec = do
  describe "Testing solution:" $ do
    it "Should double a positive integer:" $ do
      squareDigit 9119 `shouldBe` 811181
    it "Should double a negative integer" $ do
      squareDigit (-1) `shouldBe` (-1)
      squareDigit (-42) `shouldBe` (-164)
    it "Should double zero" $ do
      squareDigit (0) `shouldBe` (0)
