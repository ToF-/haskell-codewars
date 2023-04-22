module SnailSpec where

import Snail (snail)
import Test.Hspec

spec :: Spec
spec = do
  it "Zero example" $ do
    let array = [[]]
        expected = []
    snail array `shouldBe` expected
  it "First example" $ do
    let array = [[1,2,3],
                 [4,5,6],
                 [7,8,9]]
        expected = [1,2,3,6,9,8,7,4,5]
    snail array `shouldBe` expected

  it "Second example" $ do
    let array = [[1,2,3],
                 [8,9,4],
                 [7,6,5]]
        expected = [1,2,3,4,5,6,7,8,9]
    snail array `shouldBe` expected
