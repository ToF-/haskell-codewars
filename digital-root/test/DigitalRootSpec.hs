module DigitalRootSpec where

import Test.Hspec
import DigitalRoot

spec :: Spec
spec = do
  describe "Testing solution:" $ do
    it "Should compute non-recursive roots, and 0:" $ do
      digitalRoot 16 `shouldBe` 7
      digitalRoot 0 `shouldBe` 0
    it "Should compute recursive roots:" $ do
      digitalRoot 195 `shouldBe` 6
      digitalRoot 992 `shouldBe` 2
      digitalRoot 999999999999 `shouldBe` 9
