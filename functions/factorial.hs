main :: IO ()
main = print (fac 5)

-- Naive Solution (I know)

fac :: Integer -> Integer
fac i
    | i <= 0 = 1 -- Base Case
    | otherwise = i * fac (i - 1)
