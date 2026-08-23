import BasicOps

fac :: Integer -> Integer
fac i
    | i <= 0 = 0
    | otherwise = product_ [1 .. i]
