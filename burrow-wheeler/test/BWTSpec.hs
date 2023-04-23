module BWTSpec (spec, main) where

import BWT

import Test.Hspec
import Text.Printf

main = hspec spec
spec = do
    describe "basic examples for encode" $ do
        it "bananabar" $ encode "bananabar" `shouldBe` ("nnbbraaaa", 4)
        it "Humble Bundle" $ encode "Humble Bundle" `shouldBe` ("e emnllbduuHB", 2)
        it "Mellow Yellow" $ encode "Mellow Yellow" `shouldBe` ("ww MYeelllloo", 1)
    describe "basic examples for decode" $ do
        it "bananabar" $ uncurry decode ("nnbbraaaa", 4) `shouldBe` "bananabar"
        it "Humble Bundle" $ uncurry decode ("e emnllbduuHB",2) `shouldBe` "Humble Bundle"
        it "Mellow Yellow" $ uncurry decode ("ww MYeelllloo", 1) `shouldBe` "Mellow Yellow"
