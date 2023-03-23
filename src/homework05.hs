module Homework05 () where


import Data.List (elemIndices)


filter' :: Foldable t => (a -> Bool) -> t a -> [a]
filter' p = foldr satisfiesCond [] where
    satisfiesCond x xs
        | p x = x : xs
        | otherwise = xs


elem' :: Eq a => a -> [a] -> Bool
elem' e = foldr (||) False . map (== e)


elem'' :: (Foldable t, Eq a) => a -> t a -> Bool
elem'' e = foldr (\x xs -> (x == e) || xs) False


composeAll :: [a -> a] -> (a -> a)
composeAll funcs = foldr (.) id funcs


horner :: [Double] -> Double -> Double
horner coeffs val = foldr (\x xs -> (x + xs * val)) 0 coeffs


reverse' :: [a] -> [a]
reverse' = foldl (\xs x -> x : xs) []


reverse'' :: [a] -> [a]
reverse'' = foldl (flip (:)) []


findIndices :: (a -> Bool) -> [a] -> [Int]
findIndices p = elemIndices True . map p
