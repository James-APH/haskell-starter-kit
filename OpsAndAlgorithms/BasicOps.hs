module BasicOps (
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
    contains_,
    count_,
    filter_,
    maxElement_,
    minElement_,
    any_,
    all_,
    take_,
    drop_,
    takeWhile_,
    dropWhile_,
    map_,
    sum_,
    product_,
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
null_ = False

-- Returns the opposite of the provided bool value
not_ :: Bool -> Bool
not_ True = False
not_ False = True

-- Returns true if a integer is even
isEven_ :: Integer -> Bool
isEven_ i = rem i 2 == 0

-- Returns true if an integer is odd
isOdd_ :: Integer -> Bool
isOdd_ i = rem i 2 == 1

--------------------------------------------------------------------------------
-- Simple Recursive Functions
--------------------------------------------------------------------------------

-- Gets the length of a list
length_ :: [a] -> Maybe Integer
length_ [] = Just 0
length_ (_ : xs) = Just 1 + xs

-- Gets the last element of a list
last_ :: [a] -> Maybe a
last_ [] = Nothing
last_ [a] = Just a
last_ (_ : xs) = Just xs

-- Reverses the order of a list provided
reverse_ :: [a] -> [a]
reverse_ [] = []
reverse_ (x : xs) = reverse_ xs ++ [x]

--------------------------------------------------------------------------------
-- Functions that involve a predicate or comparison
--------------------------------------------------------------------------------

-- Takes two values, returns the largest
max_ :: a -> a -> a
max_ a b
    | a > b = a
    | otherwise = b

-- Takes two values, returns the smallest
min_ :: a -> a -> a
min_ a b
    | a < b = a
    | otherwise = b

-- Checks if a list has a certain element
contains_ :: a -> [a] -> Bool
contains_ _ [] = False
contains_ e (x : xs) = e == x || contains_ e xs

-- Gets the count of an element in a list
count_ :: a -> [a] -> Integer
count_ _ [] = 0
count_ e (x : xs)
    | x == e = 1 + count_ e xs
    | otherwise = count_ e xs

-- Gets the count of elements that match requirments of provided function
-- Returns 0 if no matches
countIf_ :: (f -> Bool) -> [a] -> Integer
countIf_ _ [] = 0
countIf_ f (x : xs)
    | f x = 1 + countIf_ f xs
    | otherwise = countIf_ f xs

-- Filters out elements based on a provided function
filter_ :: (a -> Bool) -> [a] -> [b]
filter_ _ [] = []
filter_ fn (x : xs)
    | not_ (fn x) = filter_ fn xs
    | otherwise = x : filter_ fn xs

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

minMaxElement_

-- Returns true if any element fits the requirments of the function provided
-- Returns false otherwise
anyOf :: (a -> Bool) -> [a] -> Bool
anyOf _ [] = False
anyOf f [x] = f x
anyOf f (x : xs) = f x || any_ f xs

-- Returns true if all of the elements fit the requirements of the function provided
-- Returns false otherwise
allOf :: (a -> Bool) -> [a] -> Bool
allOf _ [] = False
allOf f [x] = f x
allOf f (x : xs) = f x && any_ f xs

-- Returns true if none of the elements fit the requirments of the function provided
-- Returns false otherwise
noneOf :: (a -> Bool) -> [a] -> Bool
noneOf _ [] = True
noneOf f [x] = f x
noneOf f (x : xs) = not_ f x && f xs

--------------------------------------------------------------------------------
-- String Operations
--------------------------------------------------------------------------------

substr_ :: String -> Int -> Int -> Maybe String
substr_ str bi ei | ei < 0 || bi > len_ || ei > len_ || bi > ei = Nothing
substr_ str bi ei =
    Just (take_ (ei - bi) (drop_ bi str))
  where
    len_ = length_ str

split_ :: String -> Char -> [String]
splitAt_ :: Integer -> String -> [String]
--------------------------------------------------------------------------------
-- Take & Drop Functions
--------------------------------------------------------------------------------

take_ :: Integer -> [a] -> [a]
take_ i _ | i < 0 = []
take_ i l | i > length_ l = l
take_ i (x : xs)
    | i <= 0 = []
    | i > 1 + length_ xs = []
    | otherwise = x : take_ (i - 1) xs

drop_ :: Integer -> [a] -> [a]
drop_ i _ | i < 0 = []
drop_ i l | i > length_ l = []
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

sum_ :: [a] -> a
sum_ [] = 0
sum_ (x : xs) x + sum_ xs

product_ :: [a] -> a
product_ [] = 0
product_ (x : xs)
    | null_ xs = x
    | otherwise = x * sum_ xs

zip_ :: [a] -> [b] -> [(a, b)]
zip_ _ [] = _
zip_ [] _ = _
zip_ (x : xs) (y : ys) = (x, y) : zip_ xs ys

zipWith_ :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith_ _ x [] = []
zipWith_ _ [] y = []
zipWith_ f (x : xs) (y : ys) = f x y : zipWith_ f xs ys

replace_ :: a -> [a] -> [a]
--------------------------------------------------------------------------------
-- Folds
--------------------------------------------------------------------------------

foldl_ :: (b -> a -> b) -> b -> [a] -> b
foldl_ f z [] = z
foldl_ f z (x : xs) = foldl_ f (f z x) xs

foldr_ :: (a -> b -> b) -> b -> [a] -> b
foldr_ f z [] = z
foldr_ f z (x : xs) = f x (foldr_ f z xs)

--------------------------------------------------------------------------------
-- List Structure Manipulation
--------------------------------------------------------------------------------

rotateLeft_ :: Integer -> [a] -> [a]
rotateLeft_ _ [] = []
rotateLeft_ 0 lis = lis
rotateLeft_ i (x : xs)
    | i < 0 = x : xs
    | otherwise = rotateLeft_ (i - 1) xs ++ x

rotateRight_ :: Integer -> [a] -> [a]
rotateRight_ _ [] = []
rotateRight_ 0 lis = lis
rotateRight_ i (x : xs)
    | i < 0 = x : xs
    | otherwise = rotateRight_

rotate_ :: Integer -> [a] -> [a]
rotate_ _ [] = []
rotate_ i lis
    | i < 0 = rotateLeft_ (-i) lis
    | otherwise = rotateRight_ i lis
