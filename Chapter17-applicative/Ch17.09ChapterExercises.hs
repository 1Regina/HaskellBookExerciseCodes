module ChApplicatives where

import Data.Monoid 
import Control.Applicative (liftA3)

-- Section: Specialise Type
-- Q1
-- Type : []

-- Methods
-- pure :: a-> [a]
-- (<*>):: [a->b]-> [a]-> [b]


-- Q2
-- Type ; IO

-- Methods
-- pureForIO :: a -> IO a
-- appendforIO  :: IO (a -> b) -> IO a -> IO b

-- Q3
-- Type : (,) a

-- Methods
-- pureForTuple :: a -> (a , a)
-- (<*>):: (a,(a->b)) -> (a,a) -> (a. b)


-- Q4
-- Type : (->) e

-- Methods
-- pureForFunc :: a -> (e -> a)

-- (<*>):: (e -> a -> b) -> (e -> a) -> (e -> b)

-- Section Instances

-- Q1
data Pair a = Pair a a
  deriving Show

instance Functor Pair where
  fmap f (Pair a a') = Pair (f a) (f a')

instance Applicative Pair where
  pure a = Pair a a

  (Pair f g) <*> (Pair a a') = Pair (f a) (g a')


-- Q2
data Two a b = Two a b deriving Show

instance Functor (Two a) where -- Rem Tuple cases (first is untouched)
  fmap f (Two a b) = Two a  (f b)

instance Monoid a => Applicative (Two a) where
  pure b = Two mempty b 

  (Two a f) <*> (Two b b') = Two (a <> b) (f b') -- See p1042

-- Q3
data Three a b c = Three a b c deriving Show

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where 
    pure c = Three mempty mempty c

    (Three a b f) <*> (Three x y z) = Three (a <> x) (b <> y) (f z)

-- Q4
data Three' a b  = Three' a b b deriving Show

instance Functor (Three' a) where  -- Rem Tuple cases (first is untouched)
    fmap f (Three' a b b') = Three' a (f b) (f b') 

instance (Monoid a) => Applicative (Three' a ) where 
    pure b = Three' mempty b b

    (Three' a f g) <*> (Three' x y z) = Three' (a <> x) (f y) (g z)   

-- Q5
data Four a b c d = Four a b c d
  deriving Show

instance Functor (Four a b c) where  -- Rem Tuple cases (Monoid firsts are untouched)
    fmap f (Four a b c d) = Four a b c (f d)  

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where 
    pure d = Four mempty mempty mempty d

    (Four a b c f) <*> (Four a' b' c' d) = Four (a <> a') (b <> b') (c <> c') (f d)   

-- Q6
data Four' a b  = Four' a a a b  deriving Show

instance Functor (Four' a) where  
        fmap f (Four' a0 a1 a2 b) = Four' a0 a1 a2 (f b)  

instance (Monoid a) => Applicative (Four' a) where 
    pure b = Four' mempty mempty mempty b

    (Four' a b c f ) <*> (Four' a' b' c' d) = Four' (a <> a') (b <> b') (c <> c') (f d)   


-- Section Combinations
stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
-- See Ch17.04MonoidApplicative.hs
-- liftA3 :: Applicative f => 
--             (a -> b -> c -> d)
--         ->  f a
--         ->  f b
--         ->  f c
--         ->  f d
     -- subtly f is the list structure function [ ]  OR cons
combos = liftA3 (,,) 
