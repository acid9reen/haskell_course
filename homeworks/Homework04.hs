module Homework04 () where


iter :: (a -> a) -> Integer -> (a -> a)
iter f 0 = id
iter f n = f . iter f (n-1)


next :: Int -> Int
next x
  | x `mod` 2 == 0 = x `div` 2
  | otherwise = 3 * x + 1


collatzLength :: Int -> Int
collatzLength n = length . takeWhile (/= 1) $ iterate next n


longCollatz :: Int -> Int -> Int -> Int
longCollatz a b n = length . filter (>= n) $ map collatzLength [a,a+1..b]
