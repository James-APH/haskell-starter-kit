module OpsAndAlgorithms.BasicOps (
    head_,
    tail_,
    null_,
    not_,
    isEven_,
    isOdd_,
    length_,
    last_,
    reverse_,
    max_,
    min_,
    maxElement_,
    minElement_,
    minmax_,
    minmaxElement_,
    contains_,
    count_,
    countIf_,
    filter_,
    anyOf_,
    allOf_,
    noneOf_,
    stringLowercase_,
    stringUppercase_,
    stringDigits_,
    stringHexDigits_,
    stringOctDigits_,
    stringPunctuation_,
    stringLetters_,
    substr_,
    abs_,
    take_,
    drop_,
    takeWhile_,
    dropWhile_,
    map_,
    sum_,
    product_,
    zip_,
    zipWith_,
    replace_,
    foldl_,
    foldr_,
) where

--------------------------------------------------------------------------------
-- Simple Functions
--------------------------------------------------------------------------------

-- Gets the first element of a list
head_ :: [a] -> Maybe a
head_ [] = Nothing
head_ (x : _) = Just x

-- Gets the remaining element(s) of a list excluding the first
tail_ :: [a] -> Maybe [a]
tail_ [] = Nothing
tail_ (_ : xs) = Just xs

-- Returns true if a list provided is empty
null_ :: [a] -> Bool
null_ [] = True
null_ _ = False

-- Returns the opposite of the provided bool value
not_ :: Bool -> Bool
not_ True = False
not_ False = True

-- Returns true if a integer is even
isEven_ :: Integer -> Bool
isEven_ i = rem i 2 == 0

-- Returns true if an integer is odd
isOdd_ :: Integer -> Bool
isOdd_ i = rem (abs_ i) 2 == 1

--------------------------------------------------------------------------------
-- Simple Recursive Functions
--------------------------------------------------------------------------------

-- Gets the length of a list
length_ :: [a] -> Integer
length_ [] = 0
length_ (_ : xs) = 1 + length_ xs

-- Gets the last element of a list
last_ :: [a] -> Maybe a
last_ [] = Nothing
last_ [a] = Just a
last_ (_ : xs) = last_ xs

-- Reverses the order of a list provided
reverse_ :: [a] -> [a]
reverse_ [] = []
reverse_ (x : xs) = reverse_ xs ++ [x]

--------------------------------------------------------------------------------
-- Functions that involve a predicate or comparison
--------------------------------------------------------------------------------

-- Takes two values, returns the largest
max_ :: (Ord a) => a -> a -> a
max_ a b
    | a > b = a
    | otherwise = b

-- Takes two values, returns the smallest
min_ :: (Ord a) => a -> a -> a
min_ a b
    | a < b = a
    | otherwise = b

-- Returns the max element from a list, or nothing if the list is empty
maxElement_ :: (Ord a) => [a] -> Maybe a
maxElement_ [] = Nothing
maxElement_ [x] = Just x
maxElement_ (x : xs) = max_ x <$> maxElement_ xs

-- Returns the min element from a list, or nothing if the list is empty
minElement_ :: (Ord a) => [a] -> Maybe a
minElement_ [] = Nothing
minElement_ [x] = Just x
minElement_ (x : xs) = min_ x <$> minElement_ xs

-- Returns the minimum and maximum numbers provided
minmax_ :: (Ord a) => a -> a -> (a, a)
minmax_ a b = (min_ a b, max_ a b)

-- Returns the minimum and maximum elements in a list
minmaxElement_ :: (Ord a) => [a] -> Maybe (a, a)
minmaxElement_ xs = case (minElement_ xs, maxElement_ xs) of
    (Just _min, Just _max) -> Just (_min, _max)
    _ -> Nothing

-- Checks if a list has a certain element
contains_ :: (Eq a) => a -> [a] -> Bool
contains_ _ [] = False
contains_ e (x : xs) = e == x || contains_ e xs

-- Gets the count of an element in a list
count_ :: (Eq a) => a -> [a] -> Integer
count_ _ [] = 0
count_ e (x : xs)
    | x == e = 1 + count_ e xs
    | otherwise = count_ e xs

-- Gets the count of elements that match requirments of provided function
-- Returns 0 if no matches
countIf_ :: (a -> Bool) -> [a] -> Integer
countIf_ _ [] = 0
countIf_ f (x : xs)
    | f x = 1 + countIf_ f xs
    | otherwise = countIf_ f xs

-- Filters out elements based on a provided function
filter_ :: (a -> Bool) -> [a] -> [a]
filter_ _ [] = []
filter_ fn (x : xs)
    | not_ (fn x) = filter_ fn xs
    | otherwise = x : filter_ fn xs

-- Returns true if any element fits the requirments of the function provided
-- Returns false otherwise
anyOf_ :: (a -> Bool) -> [a] -> Bool
anyOf_ _ [] = False
anyOf_ f [x] = f x
anyOf_ f (x : xs) = f x || anyOf_ f xs

-- Returns true if all of the elements fit the requirements of the function provided
-- Returns false otherwise
allOf_ :: (a -> Bool) -> [a] -> Bool
allOf_ _ [] = True
allOf_ f [x] = f x
allOf_ f (x : xs) = f x && allOf_ f xs

-- Returns true if none of the elements fit the requirments of the function provided
-- Returns false otherwise
noneOf_ :: (a -> Bool) -> [a] -> Bool
noneOf_ _ [] = True
noneOf_ f (x : xs)
    | f x = False
    | null_ xs = not_ (f x)
    | otherwise = noneOf_ f xs

--------------------------------------------------------------------------------
-- String Operations
--------------------------------------------------------------------------------

stringLowercase_ :: String
stringLowercase_ = "abcdefghijklmnopqrstuvwxyz"

stringUppercase_ :: String
stringUppercase_ = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

stringDigits_ :: String
stringDigits_ = "0123456789"

stringHexDigits_ :: String
stringHexDigits_ = "0123456789abcdefABCDEF"

stringOctDigits_ :: String
stringOctDigits_ = "01234567"

stringPunctuation_ :: String
stringPunctuation_ = "#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"

stringLetters_ :: String
stringLetters_ = stringUppercase_ ++ stringLowercase_

-- Gets a substring from a string, beginning is inclusive, end is exclusive
substr_ :: String -> Integer -> Integer -> Maybe String
substr_ "" _ _ = Just ""
substr_ str bi ei
    | ei < 0 || bi > len_ || ei > len_ || bi > ei || bi < 0 = Nothing
    | otherwise = Just (take_ (ei - bi) (drop_ bi str))
  where
    len_ = length_ str

--------------------------------------------------------------------------------
-- Math Operations
--------------------------------------------------------------------------------

-- Absolute Value
abs_ :: (Ord a, Num a) => a -> a
abs_ i = if i < 0 then -i else i

--------------------------------------------------------------------------------
-- Take & Drop Functions
--------------------------------------------------------------------------------

take_ :: Integer -> [a] -> [a]
take_ _ [] = []
take_ 0 _ = []
take_ i (x : xs)
    | i <= 0 = x : xs
    | otherwise = x : take_ (i - 1) xs

drop_ :: Integer -> [a] -> [a]
drop_ _ [] = []
drop_ 0 xs = xs
drop_ i (x : xs)
    | i <= 0 = x : xs
    | otherwise = drop_ (i - 1) xs

takeWhile_ :: (a -> Bool) -> [a] -> [a]
takeWhile_ _ [] = []
takeWhile_ f (x : xs)
    | f x = x : takeWhile_ f xs
    | otherwise = []

dropWhile_ :: (a -> Bool) -> [a] -> [a]
dropWhile_ _ [] = []
dropWhile_ f (x : xs)
    | f x = dropWhile_ f xs
    | otherwise = x : xs

--------------------------------------------------------------------------------
-- Functions that apply another function to elements on a list does not compare
-- / filter
--------------------------------------------------------------------------------

map_ :: (a -> b) -> [a] -> [b]
map_ _ [] = []
map_ fn (x : xs) = fn x : map_ fn xs

sum_ :: (Num a) => [a] -> a
sum_ = foldr_ (+) 0

product_ :: (Num a) => [a] -> a
product_ = foldr_ (*) 1

zip_ :: [a] -> [b] -> [(a, b)]
zip_ _ [] = []
zip_ [] _ = []
zip_ (x : xs) (y : ys) = (x, y) : zip_ xs ys

zipWith_ :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith_ _ _ [] = []
zipWith_ _ [] _ = []
zipWith_ f (x : xs) (y : ys) = f x y : zipWith_ f xs ys

replace_ :: (Eq a) => a -> a -> [a] -> [a]
replace_ v rv = map_ (\x -> if x == v then rv else x)

--------------------------------------------------------------------------------
-- Folds
--------------------------------------------------------------------------------

foldl_ :: (b -> a -> b) -> b -> [a] -> b
foldl_ _ z [] = z
foldl_ f z (x : xs) = foldl_ f (f z x) xs

foldr_ :: (a -> b -> b) -> b -> [a] -> b
foldr_ _ z [] = z
foldr_ f z (x : xs) = f x (foldr_ f z xs)
