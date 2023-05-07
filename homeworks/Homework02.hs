fibgen :: Integer -> Integer -> Integer -> Integer
fibgen a b 0 = a
fibgen a b c = fibgen b (a + b) (c - 1)

fib :: Integer -> Integer
fib n = fibgen 0 1 n


quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (p:xs) = (quicksort lesser) ++ [p] ++ (quicksort greater)
    where
        lesser = [a | a <- xs, a < p]
        greater = [a | a <- xs, a >= p]
