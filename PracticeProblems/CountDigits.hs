import BasicOps

countDigits :: Integer -> Integer
countDigits i
    | i < 0 = countDigits (-i)
    | i < 10 = 1
    | otherwise = 1 + countDigits (div i 10)
