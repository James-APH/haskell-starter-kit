import BasicOps

fizzbuzz :: Integer -> [String]
fizzbuss 0 = "FizzBuzz"
fizzbuzz i
    | i < 0 = []
    | rem i 3 == 0 && rem i 5 == 0 = fizzbuzz (i - 1) ++ ["FizzBuzz"]
    | rem i 3 == 0 = fizzbuzz (i - 1) ++ ["Fizz"]
    | rem i 5 == 0 = fizzbuzz (i - 1) ++ ["Buzz"]
    | otherwise = fizzbuzz (i - 1) ++ [show i]
