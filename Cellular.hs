module Cellular where

import Control.Comonad

-- Zipper --

data Zipper a = Z [a] a [a] deriving Show

left  (Z [] _ _)     = Nothing
left  (Z (l:ls) c r) = Just $ Z ls l (c:r)

right (Z _ _ [])     = Nothing
right (Z l c (r:rs)) = Just $ Z (c:l) r rs

safeIterate f x = case f x of
    Nothing -> []
    Just fx -> fx : safeIterate f fx

toList (Z l c r) = reverse l ++ c : r

instance Functor Zipper where
    fmap f (Z l c r) = Z (fmap f l) (f c) (fmap f r)

instance Comonad Zipper where
    extract (Z _ c _) = c
    duplicate z@(Z l c r) = Z (safeIterate left z) z (safeIterate right z)

-- Rules --

type Rule = Int

bin :: Int -> [Int]
bin n = go [] n
      where
        go accum 0 = let n = length accum in replicate (8-n) 0 ++ accum
        go accum n = let (a,b) = divMod n 2 in go (b:accum) a

rule :: Int -> (Int,Int,Int) -> Int
rule n (a,b,c) | n < 0 || n > 255 = error "Rule must be between 0 and 255"
               | otherwise        = bin n !! (7 - 4*a - 2*b - c)

lift :: ((Int,Int,Int) -> Int) -> Zipper Int -> Int
lift f (Z []    c [])    = f (0,c,0)
lift f (Z (l:_) c [])    = f (l,c,0)
lift f (Z []    c (r:_)) = f (0,c,r)
lift f (Z (l:_) c (r:_)) = f (l,c,r)

automata :: Int -> Zipper Int -> Zipper Int
automata n z = z =>> lift (rule n)