-- A permutation is an ordered arrangement of objects. For example, 3124 is one possible permutation of the digits 1, 2, 3 and 4. If all of 
-- the permutations are listed numerically or alphabetically, we call it lexicographic order. The lexicographic permutations of 0, 1 and 2 are:

-- 012   021   102   120   201   210

-- What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?

qSort :: [Int] -> [Int]
qSort [] = []
qSort (n:ns) = qSort (filter (\x -> x <= n) (ns)) ++ [n] ++ qSort(filter (\x -> x > n) (ns))

slice :: Int -> Int -> [Int] -> [Int]
slice a b ls = drop a . take b $ ls 

toPair :: [Int] -> ([Int],[Int])
toPair ls = (reverse $ snd (toPair' [] (reverse ls)), reverse $ fst (toPair' [] (reverse ls))) where
    toPair' :: [Int] -> [Int] -> ([Int],[Int])
    toPair' [] r = toPair' ([head r]) (tail r) 
    toPair' l [] = (l,[])
    toPair' l r 
        | last l <= head r = toPair' (l ++ [head r]) (tail r) 
        | otherwise        = (l,r)

customSwap :: ([Int], [Int]) -> ([Int], [Int])
customSwap (l,r) = ((init l) ++ [temp], (f_m temp r) ++ [last l] ++ slice (length (f_m temp r) + 1) (length r) r) where
    temp = minimum (f_m (last l) r)
    f_m n rs = (filter (\x -> n < x) rs)

customConc :: ([Int],[Int]) -> [Int]
customConc (a,b) = a ++ (qSort b)

nextPermutation :: [Int] -> [Int]
nextPermutation l = customConc $ customSwap $ toPair l

nthPerm :: [Int] -> Int -> [Int]
nthPerm [] _ = []
nthPerm l 0 = l 
nthPerm l n = nthPerm (nextPermutation l) (n - 1)

problem_24 :: [Int]
problem_24 = nthPerm [0..9] 99999