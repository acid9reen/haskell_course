module Homework03 (powerset, minimum, primes, nthPrime) where


import Prelude hiding (minimum)


merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge l1@(x : xs) l2@(y : ys)
  | x < y = x : merge xs l2
  | otherwise = y : merge l1 ys


halve xs = splitAt (div (length xs) 2) xs


msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs =
  let (l1, l2) = halve xs in
    merge (msort l1) (msort l2)


powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = [x:ps | ps <- powerset xs] ++ powerset xs


-- Аналогия с Quick Select наверное уместна
-- (модификация Quick Sort для поиска k-ой статистики)
minimum :: Ord a => [a] -> a
minimum ls = head (msort ls)


primes :: [Integer]
primes = sieve $ 2 : [3,5..] where
  sieve [] = []
  sieve (x:xs) = x : sieve [n | n <- xs, n `mod` x /= 0]


nthPrime :: Int -> Integer
nthPrime n = last $ take n primes
