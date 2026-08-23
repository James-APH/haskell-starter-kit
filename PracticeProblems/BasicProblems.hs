module BasicProblems (
    gcd_,
    lcm_,
    countDigits_,
    fac_,
    fib_,
) where

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
-- Count Digits
countDigits_ :: Integer -> Integer
countDigits_ i
    | i < 0 = countDigits (-i)
    | i < 10 = 1
    | otherwise = 1 + countDigits (div i 10)

-- Factorial
fac_ :: Integer -> Integer
fac_ i
    | i <= 0 = 0
    | otherwise = product_ [1 .. i]

-- Fibonacci
fib :: Integer -> Integer
fib i
    | i <= 0 = 0
    | i == 1 = 1
    | otherwise = fib (i - 1) + fib (i - 2)

-- Sum Digits
sumDigits_ :: Integer -> Integer
sumDigits_

-- Reverse Number

--------------------------------------------------------------------------------
-- Problems with Characters & Strings
--------------------------------------------------------------------------------

-- Palindrome Checker
isP :: String -> Bool
isP str
    | null_ str || len_ == 1 = True
    | first_ str /= last_ str = False
    | otherwise = isP (substr_ str 1 (len_ - 1))
  where
    len_ = length_ str

-- Count Vowels
countVowels :: [Char] -> Integer
countVowels [] = 0
countVowels (x : xs)
    | any_ (== x) "aeiou" = 1 + countVowels xs
    | otherwise = 0 + countVowels xs

-- Count Words

--------------------------------------------------------------------------------
-- Problems with lists
--------------------------------------------------------------------------------

-- FizzBuzz
fizzbuzz :: Integer -> [String]
fizzbuss 0 = "FizzBuzz"
fizzbuzz i
    | i < 0 = []
    | rem i 3 == 0 && rem i 5 == 0 = fizzbuzz (i - 1) ++ ["FizzBuzz"]
    | rem i 3 == 0 = fizzbuzz (i - 1) ++ ["Fizz"]
    | rem i 5 == 0 = fizzbuzz (i - 1) ++ ["Buzz"]
    | otherwise = fizzbuzz (i - 1) ++ [show i]

-- Is Sorted
isSorted :: (Ord a) => [a] -> Bool
isSorted [] = True
isSorted [_] = True
isSorted (x : y : xs)
    | x <= y = isSorted (y : xs)
    | otherwise = False

-- Caesar Cipher

-- Rot13

-- Flatten

-- Remove Duplicates
