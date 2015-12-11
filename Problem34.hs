--145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.
--Find the sum of all numbers which are equal to the sum of the factorial of their digits.
--Note: as 1! = 1 and 2! = 2 are not sums they are not included.


factorial :: Int -> Int
factorial n 
    | n == 0 =  1
    | n > 0  = factorial ( n - 1 ) * n

intToListDigit :: Int -> [Int]
intToListDigit 0 = []
intToListDigit n = intToListDigit (div n 10) ++ [mod n 10]

foo :: Int -> Int
foo n = sum $ map factorial (intToListDigit n)

graph :: Int -> (Int, Int)
graph = \n -> (n, foo n)

problem_34 :: Int
problem_34 = sum $ map (fst) (filter (\p -> (fst p) == (snd p)) (map graph [3..99999]))