{-# LANGUAGE ScopedTypeVariables #-}
module Sequenze where 

import Data.Maybe   


-- Evaluate each action in the
-- structure from left to right,
-- and collect the results.
    
sequenceA :: (Applicative f, Traversable t)
              => t (f a)
              -> f (t a)
sequenceA = traverse id
--   {-# MINIMAL traverse | sequenceA #-}

-- COMPARE Section A vs B
-- Section A
-- >>> sum [1, 2, 3]
-- 6
-- >>>  fmap sum [Just 1, Just 2, Just 3]
-- [1,2,3]
-- >>>  (fmap . fmap) sum Just [1, 2, 3]
-- Just 6
-- >>>  xs = [Just 1, Just 2, Nothing]
-- >>> fmap product xs
-- [1,2,1]

-- Section B
-- >>>  fmap Just [1, 2, 3]
-- [Just 1,Just 2,Just 3]
-- >>>  sequenceA $ fmap Just [1, 2, 3]
-- Prelude Debug.SimpleReflect> sequenceA $ fmap Just [1, 2, 3]
-- Just [1,2,3]

xs = [Just 1, Just 2, Just 3]
-- Prelude Debug.SimpleReflect> sequenceA xs
-- output: Just [1,2,3]
xsn = [Just 1, Just 2, Nothing]
-- Prelude Debug.SimpleReflect> sequenceA xsn
-- output:  Nothing
-- Prelude Debug.SimpleReflect> fmap sum $ sequenceA xs
-- output: Just 6
-- Prelude Debug.SimpleReflect> fmap product (sequenceA xsn)
-- output: Nothing

--- CATMAYBES
-- >>> catMaybes xs
-- [1,2,3]
-- >>> catMaybes xsn
-- [1,2]
xsn' = xs ++ [Nothing]
-- >>> sum $ catMaybes xsn'
-- 6
-- Prelude Debug.SimpleReflect> map sum $ sequenceA xsn'
-- Nothing
