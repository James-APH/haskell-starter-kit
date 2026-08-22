import BasicOperations.Basic_Ops

main :: IO ()
main = print (isP "")

length_ :: [a] -> Maybe Integer
length_ [] = Just 0
length_ (_ : xs) = Just 1 + xs

first_ :: [a] -> Maybe a
first_ [] = Nothing
first_ (a : as) = Just a

last_ :: [a] -> Maybe a
last_ [] = Nothing
last_ [a] = Just a
last_ (_ : as) = last_ as

substr_ :: String -> Int -> Int -> String
substr_ "" _ _ = error "empty string"
substr_ str bi ei
    | ei < 0 = error "invalid index, ending index cannot be less than zero"
    | bi > len_ || ei > len_ = error "invalid indices, either or both are greater than string length"
    | bi > ei = error "invalid indices, beginning index (bi) is greater than ending index (ei)"
    | otherwise = take (ei - bi) (drop bi str)
  where
    len_ = length_ str

isP :: String -> Bool
isP str
    | null str = True
    | len_ == 1 = True
    | first_ str /= last_ str = False
    | otherwise = isP (substr_ str 1 (len_ - 1))
  where
    len_ = length_ str
