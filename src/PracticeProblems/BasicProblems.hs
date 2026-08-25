module PracticeProblems.BasicProblems (
    gcd_,
    lcm_,
    countDigits_,
    fac_,
    fib_,
    sumDigits_,
    isP_,
    countVowels_,
    fizzbuzz_,
    isSorted_,
) where

import OpsAndAlgorithms.BasicOps

--------------------------------------------------------------------------------
-- Problems with Integers / Numbers
--------------------------------------------------------------------------------

-- GCD
gcd_ :: Integer -> Integer -> Integer
gcd_ x y
    | x < 0 || y < 0 = -1
    | x == 0 || y == 0 = x + y
    | x > y = gcd_ (rem x y) y
    | y > x = gcd_ (rem y x) x
    | otherwise = x

-- LCM
lcm_ :: Integer -> Integer -> Integer
lcm_ x y
    | x < 0 || y < 0 = -1
    | x + y == 0 || x + y == x || x + y == y = 0
    | x == 1 || y == 1 = 1
    | otherwise = x * div y (gcd x y)

-- Count Digits
countDigits_ :: Integer -> Integer
countDigits_ i
    | i < 0 = countDigits_ (-i)
    | i < 10 = 1
    | otherwise = 1 + countDigits_ (div i 10)

-- Factorial
fac_ :: Integer -> Integer
fac_ i
    | i <= 0 = 0
    | otherwise = product_ [1 .. i]

-- Fibonacci
fib_ :: Integer -> Integer
fib_ i
    | i <= 0 = 0
    | i == 1 = 1
    | otherwise = fib_ (i - 1) + fib_ (i - 2)

-- Sum Digits
sumDigits_ :: Integer -> Integer
sumDigits_ i
    | i < 0 = sumDigits_ (abs_ i)
    | i < 10 = i
    | otherwise = rem i 10 + sumDigits_ (div i 10)

--------------------------------------------------------------------------------
-- Problems with Characters & Strings
--------------------------------------------------------------------------------

-- Palindrome Checker
isP_ :: String -> Bool
isP_ str
    | null_ str || len_ == 1 = True
    | head_ str /= last_ str = False
    | otherwise = maybe False isP_ (substr_ str 1 (len_ - 1))
  where
    len_ = length_ str

-- Count Vowels
countVowels_ :: [Char] -> Integer
countVowels_ [] = 0
countVowels_ (x : xs)
    | anyOf_ (== x) "aeiou" = 1 + countVowels_ xs
    | otherwise = 0 + countVowels_ xs

--------------------------------------------------------------------------------
-- Problems with lists
--------------------------------------------------------------------------------

-- FizzBuzz
fizzbuzz_ :: Integer -> [String]
fizzbuzz_ i | i < 0 = []
fizzbuzz_ 0 = []
fizzbuzz_ i =
    fizzbuzz_ (i - 1)
        ++ [ case (rem i 3, rem i 5) of
                (0, 0) -> "FizzBuzz"
                (0, _) -> "Fizz"
                (_, 0) -> "Buzz"
                (_, _) -> show i
           ]

-- Is Sorted
isSorted_ :: (Ord a) => [a] -> Bool
isSorted_ [] = True
isSorted_ [_] = True
isSorted_ (x : y : xs)
    | x <= y = isSorted_ (y : xs)
    | otherwise = False
