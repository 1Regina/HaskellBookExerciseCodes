module FoldingInstances where

import Data.Foldable
import Data.Monoid

-- a minimal Foldable instance must have either foldr or foldMap

-- Different Foldable instances

-- IDENTITY
data Identity a = Identity a

instance Foldable Identity where
    foldr   f z (Identity x) = f x z
    foldl   f z (Identity x) = f z x
    foldMap f (Identity x)   = f x

class Functor f => Foldable f where
    fold    :: Monoid m =>             f m -> m
    foldMap :: Monoid m => (a -> m) -> f a -> m
    foldMap g a = fold $ fmap g a    

-- Prelude> foldr (*) 1 (Identity 5)
-- 5
-- Prelude> foldl (*) 5 (Identity 5)
-- 25

-- Prelude> fm = foldMap (*5)
-- Prelude> type PI = Product Integer
-- Prelude> fm (Identity 100) :: PI
-- Product {getProduct = 500}


-- MAYBE
-- Need to account for the Nothing cases.  For foldr and foldl, that "zero value" is the start value.
-- Prelude> foldr (+) 1 Nothing
-- 1

-- important
-- for foldMap, we use the Monoid’s identity value as our zero
-- Prelude> fm = foldMap (+1)
-- Prelude> fm Nothing :: Sum Integer
-- Sum {getSum = 0}

-- For Just value, need to apply folding function.
-- Prelude> foldr (+) 1 (Just 3)
-- 4
-- Prelude> fm $ Just 3 :: Sum Integer
-- Sum {getSum = 4}

-- a Nada value and a declared type of Sum Int(giving us our Monoid), foldMap gives us Sum 0, because that is the mempty, or identity, for Sum. Similarly with Nada and Product, we get Product 1, because that is the identity for Product.

-- Prelude> import Data.Monoid
-- Prelude> foldMap (+1) Nada :: Sum Int
-- Sum {getSum = 0}
-- Prelude> foldMap (+1) Nada :: Product Int
-- Product {getProduct = 1}
-- Prelude> foldMap (+1) (Just 1) :: Sum Int
-- Sum {getSum = 2}


-- List
toList :: t a -> [a]

-- >>> toList (Just 1)
-- [1]
-- Prelude> xs = [Just 1, Just 2, Nothing]
-- Prelude> concatMap toList xs[1,2]
-- Prelude> toList (1, 2)
-- [2]
-- Missing 1 in the list:  same reason that fmap doesn’t apply a function to the 1.

null :: t a -> Bool
-- null returns True on Left and Nothing values, just as it does on empty lists and so forth:
-- >>> null (Left 3)
-- True

-- Prelude> null []
-- True
-- Prelude> null Nothing
-- True
-- Prelude> null (1, 2)
-- False
-- Prelude> xs = [Just 1, Just 2, Nothing]
-- Prelude> fmap null xs
-- [False,False,True]


-- for tuples, the first argument (as well as the leftmost, or outermost, type argument of datatypes such as Maybe and Either) is part of the there, not part of the a.

length :: t a -> Int
-- Prelude> length (1, 2)
-- 1
-- Prelude> xs = [(1, 2), (3, 4), (5, 6)]
-- Prelude> length xs
-- 3
-- Prelude> fmap length xs
-- [1,1,1]
-- Prelude> fmap length Just [1, 2, 3] 
-- 1  -- QUEER?
-- Prelude> length $ Just [1, 2, 3]
-- 1 -- QUEER?
-- bcos the a of Just a in the last case above is a list

-- Prelude> xs = [Just 1, Just 2, Just 3]
-- Prelude> fmap length xs
-- [1,1,1]
-- Prelude> xs = [Just 1, Just 2, Nothing]
-- Prelude> fmap length xs
-- [1,1,0]

elem :: Eq a => a -> t a -> Bool
-- Functor, you can’t map over the left data constructor,because the left type argument is part of the structure. elem can’t see inside the Left constructor to whatever the value is, so the result will be False, even if the value matches:
-- Prelude> elem 2 (Just 3)
-- False
-- Prelude> elem True (Left False)
-- False
-- Prelude> elem True (Left True)
-- False
-- Prelude> elem True (Right False)
-- False
-- Prelude> elem True (Right True)
-- True
-- Prelude> xs = [Right 1,Right 2,Right 3]
-- Prelude> fmap (elem 3) xs
-- [False,False,True]

maximum :: Ord a => t a -> a
minimum :: Ord a => t a -> a

-- notice that Left and Nothing(and similar) values are emptyfor the purposes of these functions:
-- Prelude> maximum [10, 12, 33, 5]
-- 33
-- Prelude> xs = [Just 2, Just 10, Just 4]
-- Prelude> fmap maximum xs
-- [2,10,4]
-- Prelude> fmap maximum (Just [3, 7, 10, 2])
-- Just 10
-- Prelude> minimum "julie"
-- 'e'
-- Prelude> fmap minimum (Just "julie")
-- Just 'e'
-- Prelude> xs = map Just "jul"
-- Prelude> xs
-- [Just 'j',Just 'u',Just 'l']
-- Prelude> fmap minimum xs
-- "jul"

-- Prelude> xs = [Just 4, Just 3, Nothing]
-- Prelude> fmap minimum xs
-- [4,3,*** Exception:minimum: empty structure
-- Prelude> minimum (Left 3)
-- *** Exception: minimum: empty structure

sum     :: (Foldable t, Num a) => t a -> a
product :: (Foldable t, Num a) => t a -> a

-- Prelude> sum (7, 5)
-- 5
-- Prelude> fmap sum [(7, 5), (3, 4)]
-- [5,4]
-- Prelude> fmap sum (Just [1, 2, 3, 4, 5])
-- Just 15
-- Prelude> product Nothing
-- 1
-- Prelude> fmap product (Just [])
-- Just 1
-- Prelude> fmap product (Right [1, 2, 3])
-- Right 6
-- Prelude> fmap product  [(7, 5), (3, 4)]
-- [5,4]