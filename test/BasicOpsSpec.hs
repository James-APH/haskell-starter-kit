module BasicOpsSpec where

import BasicOps
import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "head_" $ do
        it "returns the first element" $ do
            head_ [1, 2, 3] `shouldBe` 1

        it "returns an empty list if the list is emtpy" $ do
            head_ [] `shouldBe` Nothing
