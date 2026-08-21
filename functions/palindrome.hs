main :: IO ()
main = print (isP "")

-- Source - https://stackoverflow.com/a/29266342
-- Posted by J. Abrahamson
-- Retrieved 2026-08-21, License - CC BY-SA 3.0
first_ :: [a] -> Maybe a
first_ [] = Nothing
first_ (a : as) = Just a

last_ :: [a] -> Maybe a
last_ [] = Nothing
last_ [a] = Just a
last_ (_ : as) = last_ as

substr :: String -> Int -> Int -> String
substr "" _ _ = error "empty string"
substr str bi ei
    | ei < 0 = error "invalid index, ending index cannot be less than zero"
    | bi > len_ || ei > len_ = error "invalid indices, either or both are greater than string length"
    | bi > ei = error "invalid indices, beginning index (bi) is greater than ending index (ei)"
    | otherwise = take (ei - bi) (drop bi str)
  where
    len_ = length str

isP :: String -> Bool
isP str
    | null str = True
    | len_ == 1 = True
    | first_ str /= last_ str = False
    | otherwise = isP (substr str 1 (len_ - 1))
  where
    len_ = length str
