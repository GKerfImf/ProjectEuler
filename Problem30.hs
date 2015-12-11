--Surprisingly there are only three numbers that can be written as the sum of fourth powers of their digits:
--    1634 = 1^4 + 6^4 + 3^4 + 4^4
--    8208 = 8^4 + 2^4 + 0^4 + 8^4
--    9474 = 9^4 + 4^4 + 7^4 + 4^4

--As 1 = 14 is not a sum it is not included.
--The sum of these numbers is 1634 + 8208 + 9474 = 19316.
--Find the sum of all the numbers that can be written as the sum of fifth powers of their digits.

toDigit :: Int -> [Int]
toDigit 0 = []
toDigit n = toDigit (div n 10) ++ [mod n 10]

sumOfKPowersOfDigits :: Int -> Int -> Int
sumOfKPowersOfDigits n k = sum $ map (^k) (toDigit n)

sumOfFifthPowersOfDigits :: Int -> Int
sumOfFifthPowersOfDigits n = sumOfKPowersOfDigits n 5

predicate :: Int -> Bool
predicate n = n == sumOfFifthPowersOfDigits n

predicate' :: Int -> Bool
predicate' n = n == sumOfKPowersOfDigits n 4

problem_30 :: Int
problem_30 = sum $ filter predicate [2..999999]