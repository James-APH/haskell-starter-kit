import BasicOps

-- replacing the larger of the two numbers by its remainder when divided by
-- the smaller of the two (with this version, the algorithm stops when reaching
-- a zero remainder). With this improvement, the algorithm never requires more
-- steps than five times the number of digits (base 10) of the smaller integer.

gcd_ :: Integer -> Integer -> Integer
gcd_ x y
    | x < 0 || y < 0 = -1
    | x == 0 || y == 0 = x + y
    | x > y = gcd_ (rem x y) y
    | y > x = gcd_ (rem y x) x
    | otherwise = x
