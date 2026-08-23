import BasicOps

substr_ :: String -> Int -> Int -> Maybe String
substr_ str bi ei | ei < 0 || bi > len_ || ei > len_ || bi > ei = Nothing
substr_ str bi ei =
    Just (take_ (ei - bi) (drop_ bi str))
  where
    len_ = length_ str

isP :: String -> Bool
isP str
    | null_ str || len_ == 1 = True
    | first_ str /= last_ str = False
    | otherwise = isP (substr_ str 1 (len_ - 1))
  where
    len_ = length_ str
