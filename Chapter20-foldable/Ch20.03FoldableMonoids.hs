{-# LANGUAGE KindSignatures #-}
module Foldings where

import Data.Foldable
import Data.Monoid
-- import Debug.SimpleReflect

class Foldable (t:: * -> *) where
    fold    :: Monoid m => t m -> m
    foldMap :: Monoid m => (a -> m) -> t a -> m

-- ghciWith simple-reflect
-- import Debug.SimpleReflect
-- Prelude Debug.SimpleReflect> mapM_ print $ reduction $ foldr (+) 0 [1..5] 
-- 1 + (2 + (3 + (4 + (5 + 0))))
-- 1 + (2 + (3 + (4 + 5)))
-- 1 + (2 + (3 + 9))
-- 1 + (2 + 12)
-- 1 + 14
-- 15

-- FOLD
-- Compare with fold which needs to specify a Monoid Instance
-- Prelude > xs = map Sum [1..5]
-- Prelude > fold xs 
-- Sum {getSum = 15}
-- Altenative writing as:

-- Prelude> :{
-- *Main| let xs :: [Sum Integer]
-- *Main|xs = [1, 2, 3, 4, 5]
-- *Main| :}
-- Prelude> fold xs
-- Sum {getSum = 15}
-- Prelude> :{
-- *Main| let xs :: [Product Integer]
-- *Main|xs = [1, 2, 3, 4, 5]
-- *Main| :}
-- Prelude> fold xs
-- Product {getProduct = 120}

-- Sometimes REPL is smart enough to identify
-- Prelude> foldr (++) "" ["hello", " julie"]
-- "hello julie"
-- Prelude> fold ["hello", " julie"]
-- "hello julie"

-- FOLDMAP
-- foldMap must explicitly map each element of a foldable structure to a Monoid
-- Prelude> foldMap Sum [1, 2, 3, 4]
-- Sum {getSum = 10}
-- Prelude> foldMap Product [1, 2, 3, 4]
-- Product {getProduct = 24}

-- Prelude> foldMap All [True, False, True]
-- All {getAll = False}

-- Prelude> foldMap Any [(3 == 4), (9 > 5)]
-- Any {getAny = True}
-- Prelude> xs = [Just 1, Nothing, Just 5]
-- Prelude> foldMap First xs
-- First {getFirst = Just 1}
-- Prelude> foldMap Last xs
-- Last {getLast = Just 5}

-- Prelude> xs = map Product [1..3]
-- Prelude> foldMap (*5) xs
-- Product {getProduct = 750}
-- -- 5 * 10 * 15
-- 750
-- Prelude> xs = map Sum [1..3]
-- Prelude> foldMap (*5) xs
-- Sum {getSum = 30}
-- 5 + 10 + 15
-- 30

-- Compare FOLDR with function has Monoid instance baked in:
-- Prelude> foldr (*) 5 [1, 2, 3]
-- (1 * (2 * (3 * 5)))
-- 30

-- Prelude> sumXs = map Sum [2..4]
-- Prelude> foldr (*) 3 sumXs
-- Sum {getSum = 72}
-- Prelude> productXs = map Product [2..4]
-- Prelude> foldr (*) 3 productXs
-- Product {getProduct = 72}

--  if what you’re trying to fold only contains one value, declaring a Monoid instance won’t change the behavior of foldMap, either:

-- Prelude> fm = foldMap (*5)
-- Prelude> fm (Just 100) :: Product Integer
-- Product {getProduct = 500}
-- Prelude> fm (Just 5) :: Sum Integer
-- Sum {getSum = 25}

-- when only 1 value, there is nothing to mappend. Instead the mempty value from Monoid instance is applied:

-- Prelude> fm Nothing :: Sum Integer
-- Sum {getSum = 0}
-- Prelude> fm Nothing :: Product Integer
-- Product {getProduct = 1