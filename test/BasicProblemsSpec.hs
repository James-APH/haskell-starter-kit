module BasicProblemsSpec where

import PracticeProblems.BasicProblems
import Test.Hspec

spec :: Spec
spec = do
    describe "gcd_" $ do
        it "returns the greatest common divisor" $
            gcd_ 48 18 `shouldBe` 6

        it "works when the first number is smaller" $
            gcd_ 18 48 `shouldBe` 6

        it "handles equal values" $
            gcd_ 12 12 `shouldBe` 12

        it "handles one as an argument" $
            gcd_ 1 7 `shouldBe` 1

        it "handles zero" $
            gcd_ 0 5 `shouldBe` 5

        it "returns zero for two zeros" $
            gcd_ 0 0 `shouldBe` 0

        it "returns -1 for a negative first argument" $
            gcd_ (-5) 10 `shouldBe` (-1)

        it "returns -1 for a negative second argument" $
            gcd_ 5 (-10) `shouldBe` (-1)

    describe "lcm_" $ do
        it "returns the least common multiple" $
            lcm_ 4 6 `shouldBe` 12

        it "works when the first number is smaller" $
            lcm_ 3 8 `shouldBe` 24

        it "handles equal values" $
            lcm_ 7 7 `shouldBe` 7

        it "handles one as the first argument" $
            lcm_ 1 5 `shouldBe` 1

        it "handles one as the second argument" $
            lcm_ 5 1 `shouldBe` 1

        it "returns zero when the first argument is zero" $
            lcm_ 0 5 `shouldBe` 0

        it "returns zero when the second argument is zero" $
            lcm_ 5 0 `shouldBe` 0

        it "returns zero for two zeros" $
            lcm_ 0 0 `shouldBe` 0

        it "returns -1 for a negative first argument" $
            lcm_ (-4) 6 `shouldBe` (-1)

        it "returns -1 for a negative second argument" $
            lcm_ 4 (-6) `shouldBe` (-1)

    describe "countDigits_" $ do
        it "counts the digits in a positive number" $
            countDigits_ 12345 `shouldBe` 5

        it "counts one digit numbers" $
            countDigits_ 7 `shouldBe` 1

        it "counts zero as one digit" $
            countDigits_ 0 `shouldBe` 1

        it "counts digits in a negative number" $
            countDigits_ (-12345) `shouldBe` 5

        it "handles a large number" $
            countDigits_ 1000000 `shouldBe` 7

    describe "fac_" $ do
        it "calculates factorial" $
            fac_ 5 `shouldBe` 120

        it "calculates factorial of one" $
            fac_ 1 `shouldBe` 1

        it "returns zero for zero" $
            fac_ 0 `shouldBe` 0

        it "returns zero for a negative number" $
            fac_ (-5) `shouldBe` 0

        it "calculates a larger factorial" $
            fac_ 10 `shouldBe` 3628800

    describe "fib_" $ do
        it "returns zero for zero" $
            fib_ 0 `shouldBe` 0

        it "returns one for one" $
            fib_ 1 `shouldBe` 1

        it "calculates Fibonacci numbers" $
            fib_ 10 `shouldBe` 55

        it "calculates a small Fibonacci number" $
            fib_ 5 `shouldBe` 5

        it "returns zero for a negative number" $
            fib_ (-5) `shouldBe` 0

        it "calculates a larger Fibonacci number" $
            fib_ 20 `shouldBe` 6765

    describe "sumDigits_" $ do
        it "sums the digits of a number" $
            sumDigits_ 12345 `shouldBe` 15

        it "handles a single digit" $
            sumDigits_ 7 `shouldBe` 7

        it "handles zero" $
            sumDigits_ 0 `shouldBe` 0

        it "handles repeated digits" $
            sumDigits_ 11111 `shouldBe` 5

        it "handles negative numbers" $
            sumDigits_ (-12345) `shouldBe` 15

    describe "isP_" $ do
        it "returns True for a palindrome" $
            isP_ "racecar" `shouldBe` True

        it "returns False for a non-palindrome" $
            isP_ "hello" `shouldBe` False

        it "handles a single character" $
            isP_ "a" `shouldBe` True

        it "handles an empty string" $
            isP_ "" `shouldBe` True

        it "handles an even-length palindrome" $
            isP_ "abba" `shouldBe` True

        it "handles an even-length non-palindrome" $
            isP_ "abca" `shouldBe` False

        it "handles repeated characters" $
            isP_ "aaaa" `shouldBe` True

    describe "countVowels_" $ do
        it "counts vowels in a string" $
            countVowels_ "hello" `shouldBe` 2

        it "counts all five vowels" $
            countVowels_ "aeiou" `shouldBe` 5

        it "counts repeated vowels" $
            countVowels_ "aaa" `shouldBe` 3

        it "returns zero when there are no vowels" $
            countVowels_ "rhythms" `shouldBe` 0

        it "returns zero for an empty string" $
            countVowels_ "" `shouldBe` 0

        it "only counts lowercase vowels" $
            countVowels_ "AEIOU" `shouldBe` 0

    describe "fizzbuzz_" $ do
        it "returns an empty list for zero" $
            fizzbuzz_ 0 `shouldBe` []

        it "returns an empty list for a negative number" $
            fizzbuzz_ (-5) `shouldBe` []

        it "returns numbers for values that are not divisible by three or five" $
            fizzbuzz_ 2 `shouldBe` ["1", "2"]

        it "returns Fizz for multiples of three" $
            fizzbuzz_ 3 `shouldBe` ["1", "2", "Fizz"]

        it "returns Buzz for multiples of five" $
            fizzbuzz_ 5 `shouldBe` ["1", "2", "Fizz", "4", "Buzz"]

        it "returns FizzBuzz for multiples of three and five" $
            fizzbuzz_ 15
                `shouldBe` [ "1"
                           , "2"
                           , "Fizz"
                           , "4"
                           , "Buzz"
                           , "Fizz"
                           , "7"
                           , "8"
                           , "Fizz"
                           , "Buzz"
                           , "11"
                           , "Fizz"
                           , "13"
                           , "14"
                           , "FizzBuzz"
                           ]

    describe "isSorted_" $ do
        it "returns True for a sorted list" $
            isSorted_ ([1, 2, 3, 4, 5] :: [Integer]) `shouldBe` True

        it "returns False for an unsorted list" $
            isSorted_ ([1, 3, 2, 4, 5] :: [Integer]) `shouldBe` False

        it "returns True for an empty list" $
            isSorted_ ([] :: [Integer]) `shouldBe` True

        it "returns True for a singleton list" $
            isSorted_ ([42] :: [Integer]) `shouldBe` True

        it "handles equal values" $
            isSorted_ ([1, 1, 2, 2, 3] :: [Integer]) `shouldBe` True

        it "handles negative numbers" $
            isSorted_ ([-5, -3, -1, 0, 2] :: [Integer]) `shouldBe` True

        it "handles a descending list" $
            isSorted_ ([5, 4, 3, 2, 1] :: [Integer]) `shouldBe` False
