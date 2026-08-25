module Main where

import qualified BasicOpsSpec
import qualified BasicProblemsSpec

import Test.Hspec

main :: IO ()
main = hspec $ do
    BasicOpsSpec.spec
    BasicProblemsSpec.spec
