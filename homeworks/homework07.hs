module Homework07 () where

import Data.Function (on)

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
--  deriving Show

instance Num Exp where
  (+) = Add
  (*) = Mul
  (-) = Sub
  fromInteger = Const . fromInteger :: Integer -> Exp
  abs = undefined
  signum = undefined

e1 :: Exp
e1 = 1+(5-3)*(1+2+4)

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

applyDistr :: Exp -> Exp
applyDistr (Mul x (Add y z)) = (Add `on` applyDistr) (Mul x y) (Mul x z) 
applyDistr (Mul x (Sub y z)) = (Sub `on` applyDistr) (Mul x y) (Mul x z) 
applyDistr (Mul x y) = (Mul `on` applyDistr) x y
applyDistr (Add x y) = (Add `on` applyDistr) x y
applyDistr (Sub x y) = (Sub `on` applyDistr) x y
applyDistr exp = exp
