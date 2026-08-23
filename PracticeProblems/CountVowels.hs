import BasicOps

countVowels :: [Char] -> Integer
countVowels [] = 0
countVowels (x : xs)
    | any_ (== x) "aeiou" = 1 + countVowels xs
    | otherwise = 0 + countVowels xs
