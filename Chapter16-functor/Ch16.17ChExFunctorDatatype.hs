{-# LANGUAGE FlexibleInstances #-}

import GHC.Arr (Array)

---
-- Q1
data Bool'' = False'' | True''

-- No Functor instance because kind is *


-- Q2

data BoolAndSomethingElse a = False' a | True' a  deriving Show

instance Functor BoolAndSomethingElse where
  fmap f (False' a) = False' (f a)
  fmap f (True'  a) = True'  (f a)


-- Q3

data BoolAndMaybeSomethingElse a = Falsish | Truish a
  deriving Show

instance Functor BoolAndMaybeSomethingElse where
  fmap _ Falsish    = Falsish
  fmap f (Truish a) = Truish (f a)

 -- Q4
newtype Mu f = InF { outF :: f (Mu f) }

-- No Functor. :Kind is: (* -> *) -> * 
-- *FunctorInstances> :k Functor
-- Functor :: (* -> *) -> Constraint

-- Q5
data D = D (Array Word Word) Int Int

-- No Functor? Kind is: *

-- The f in the typeclass definition for Functor has kind * -> *.

-- Each argument in the type signature for a function (e.g., fmap) must be fully applied, which is to say, must have kind *


