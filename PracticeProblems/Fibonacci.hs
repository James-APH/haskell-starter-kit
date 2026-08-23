import BasicOps

fib :: Integer -> Integer
fib i
    | i <= 0 = 0
    | i == 1 = 1
    | otherwise = fib (i - 1) + fib (i - 2)
