module Midterm (hIndex, abbrev, interesting) where

import Data.List (repeat, sort, group, groupBy)
import Data.Function (on)


-- Задание 1
-- Заводится счетчик для каждого элемента списка (пара - (элемент, кол-во вхождений))
-- (достигается сортировкой и группировкой),
-- затем отбрасываются пары, первый элемент которых не может быть индексом Хирша,
-- из-за сортировки, максимальный индекс будет располагаться в конце списка,
-- берем его и достаем из него первый элемент - индекс Хирша
hIndex :: [Int] -> Int
hIndex = fst . last . filter (\(x, y) -> x <= y) . counter where
  counter = map (\x -> (head x, length x)) . group . sort


-- Задание 2
abbrev :: String -> String
abbrev = foldr1 (\(h:_) xs -> h : '.' : xs) . words


-- Задание 3
-- Вне базового случая рассматриваем неравенство x < y
-- где x - голова первого списка, y - второго,
-- если оно верно, то конструируем список из x и рекурсивного вызова
-- с нераспределенными остатками
-- иначе конструируем список из y и рекурсивного вызова с остатками
mergeBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
mergeBy _ x [] = x
mergeBy _ [] y = y
mergeBy comp xs@(x:txs) ys@(y:tys)
  | (comp x y) == LT = x : (mergeBy comp txs ys)
  | otherwise = y : (mergeBy comp tys xs)

-- Вне базового случая конструируем список из (x, y), где
-- x - голова первого списка, y - второго, и
-- вызова mergeBy по критерию w для оставшихся пар (a, b) из xts и yts
--
-- Голову нужно отщиплять отдельно для того, чтобы эта функция могла вычисляться лениво
weightedPairs :: Ord c => ((a, b) -> c) -> [a] -> [b] -> [(a, b)]
weightedPairs _ [] _ = []
weightedPairs _ _ [] = []
weightedPairs w (x:xts) (y:yts) = (x, y) :
  mergeBy (compare `on` w) [(x, y) | y <- yts] (weightedPairs w xts yts)

weight :: (Integer, Integer) -> Integer
weight (i, j) = i^3 + j^3

interesting :: [(Integer, [(Integer, Integer)])]
interesting = [(weight $ head lst, lst) | lst <- lsts] where
  lsts = filter ((> 1) . length) $ groupBy ((==) `on` weight) $ weightedPairs weight [1..] [1..]

-- Первые 6 интересных чисел и пар их образующих
--  1729, [( 1,12), ( 9,10)]
--  4104, [( 2,16), ( 9,15)]
-- 13832, [( 2,24), (18,20)]
-- 20683, [(10,27), (19,24)]
-- 32832, [( 4,32), (18,30)]
-- 39312, [( 2,34), (15,33)]


-- Задание 4
data Nat = Zero | Succ Nat deriving Show

toPeano 0 = Zero
toPeano n = Succ $ toPeano (n-1)

fromPeano Zero = 0
fromPeano (Succ x) = 1 + fromPeano x

add :: Nat -> Nat -> Nat
add Zero y = y
add (Succ x) y = Succ (add x y)

mul :: Nat -> Nat -> Nat
mul Zero _ = Zero
mul _ Zero  = Zero
mul (Succ x) y = add y $ mul x y


data Exp =
  Const Int
  | Add Exp Exp
  | Sub Exp Exp
  | Mul Exp Exp


instance Num Exp where
  (+) = Add
  (*) = Mul
  (-) = Sub
  fromInteger = Const . fromInteger :: Integer -> Exp
  abs = undefined
  signum = undefined

showExp :: Exp -> String
showExp (Const x) = show x
showExp (Add x y) = "(" ++ (showExp x) ++ "+" ++ (showExp y) ++ ")"
showExp (Mul x y) = "(" ++ (showExp x) ++ "*" ++ (showExp y) ++ ")"
showExp (Sub x y) = "(" ++ (showExp x) ++ "-" ++ (showExp y) ++ ")"

instance Show Exp where
  show = showExp

eval :: Exp -> Int
eval (Const x) = x
eval (Add x y) = ((+) `on` eval) x y
eval (Mul x y) = ((*) `on` eval) x y
eval (Sub x y) = ((-) `on` eval) x y

-- К выражениям вида (Mul ([Add | Sub] x y)) применяем дистрибутивность
-- и рекурсивно применяем дистрибутивность к обоим слагаемым
--
-- К выражениям вида ([Mul | Sub | Add] x y) просто применяем дистрибутивность
-- к x и y
--
-- Иначе (по сути случай константы) - возвращаем данное выражение
applyDistr :: Exp -> Exp
applyDistr (Mul x (Add y z)) = (Add `on` applyDistr) (Mul x y) (Mul x z)
applyDistr (Mul x (Sub y z)) = (Sub `on` applyDistr) (Mul x y) (Mul x z)
applyDistr (Mul x y) = (Mul `on` applyDistr) x y
applyDistr (Add x y) = (Add `on` applyDistr) x y
applyDistr (Sub x y) = (Sub `on` applyDistr) x y
applyDistr exp = exp

e1 :: Exp
e1 = (5-3)*(1+2+4)


-- Задание 5

-- myFoldl :: (a -> b -> a) -> a -> [b] -> a
-- myFoldl f z xs = foldr (\x g y -> g (f y x)) id xs z

-- f :: a -> b -> a
-- z :: a
-- xs :: [b]
-- x :: b
-- g :: a -> a
-- y :: a
-- f y :: b -> a
-- f y x :: a
-- g (f y x) :: a
-- \x g y -> g (f y x) :: b -> (a -> a) -> a -> a
-- id :: a -> a
-- foldr :: (b -> (a -> a) -> (a -> a)) -> (a -> a) -> [b] -> (a -> a)
-- foldr (\x g y -> g (f y x)) :: (a -> a) -> [b] -> (a -> a)
-- foldr (\x g y -> g (f y x)) id :: [b] -> (a -> a)
-- foldr (\x g y -> g (f y x)) id xs :: a -> a
-- foldr (\x g y -> g (f y x)) id xs z :: a


-- Задание 6
newtype Endo a = Endo { appEndo :: a -> a }

instance Semigroup (Endo a) where
  Endo f <> Endo g = Endo (f . g)

instance Monoid (Endo a) where
  mempty = Endo id


concatS :: [ShowS] -> String
concatS ss = appEndo (foldMap Endo ss) ""

a = ["abc", "de", "f", "ghi"]
b = map showString a
