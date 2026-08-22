main :: IO ()
main = print (fib 8)

-- Naive Solution (I know)

fib :: Integer -> Integer
fib i
    | i <= 0 = 0 -- Base Case
    | i == 1 = 1
    | otherwise = fib (i - 1) + fib (i - 2)
