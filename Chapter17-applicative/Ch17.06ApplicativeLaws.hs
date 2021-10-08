-- 1. Identity: pure id <*> v = v
-- 2. Composition: pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
-- 3. Homomorphism: pure f <*> pure x = pure (f x)
-- 4. Interchange: u <*> pure y = pure ($ y) <*> u

-- p1081
-- S1 Identity
-- class Functor f => Applicative (f :: * -> *) where

-- pure :: a -> f a
-- pure id <*> v = v


-- >>> pure id <*> [1..5]
-- [1,2,3,4,5]
--
-- >>> pure id <*> Just "Hello Applicative"
-- Just "Hello Applicative"
--
-- >>> pure id <*> Nothing
-- Nothing
--
-- >>> pure id <*> Left "Error'ish"
-- Left "Error'ish"
--
-- >>> pure id <*> Right 8001
-- Right 8001
--
-- >>> pure id <*> (+1) $ 2
-- 3
--
-- >>> id[1..5]
-- [1,2,3,4,5]
--
-- >>> fmap id [1..5]
-- [1,2,3,4,5]
--
-- >>> pure id <*> [1..5]
-- [1,2,3,4,5]
--
-- >>>  (fmap id [1..5]) ==  (pure id <*> [1..5])
-- True
--

-- S2 Composition

-- pure (.) <*> u <*> v <*> w = u <*> (v <*> w)

-- pure (.) <*> [(+1)] <*> [(*2)] <*> [1, 2, 3]
-- >>> [(+1)]<*>([(*2)]<*>[1, 2, 3])
-- [3,5,7]

-- pure (.) <*> Just (+1) <*> Just (*2) <*> Just 1 
-- >>> Just (+1) <*> (Just (*2) <*> Just 1)
-- Just 3
--

-- S3 Homomorphism -- a structure-preserving map betweentwo algebraic structures
-- pure f <*> pure x = pure (f x)

-- >>> pure (+1) <*> pure 1
-- 2
--
-- >>> pure ((+1) 1)
-- 2
--
-- >>> (+1) 1
-- 2
--
-- >>> pure (+1) <*> pure 1 :: Maybe Int
-- Just 2
-- >>> pure (+1) <*> pure 1 :: [Int]
-- [2]
--
-- >>> pure (+1) <*> pure 1 :: Either a Int
-- Right 2
--
-- >>> pure ((+1) 1) :: Maybe Int
-- Just 2
--
-- >>> (+1) 1
-- 2
-- 

-- S4 Interchange

-- u <*> pure y = pure ($ y) <*> u

-- u is function embedded in a structure. left of <*> must always be a function embedded in some structure.
-- >>> Just (+2) <*> pure 2
-- Just 4

-- Pattern match
-- Just (+2) <*> pure 2
-- --  u     <*> pure y
-- -- equals
-- Just 4

-- >>> pure ($ 2) <*> Just (+ 2)
-- Just 4
--
mPure :: a -> Maybe a 
mPure = pure
embed :: Num a => Maybe ((a -> b) -> b)
embed = mPure ($2)
mApply:: Maybe ((a -> b) -> b)
      -> Maybe (a -> b)
      -> Maybe              b
mApply = (<*>) -- bcos same as :t applicative <*>

myResult = pure ( $ 2) `mApply` Just (+2)
-- >>> myResult
-- Just 4
--
-- Interchange law


-- Test for interchange 
-- (Just (+2) <*> pure 2) ==  (pure ($2) <*> Just (+2))
-- >>> (Just (+2) <*> pure 2) ==  (pure ($2) <*> Just (+2))
-- True
--
-- Extra test p1089
-- >>> pure 1
-- 1
--
-- Composition and Interchange, Maybe Homomorphism
-- >>> [(+1), (*2)] <*> pure 1
-- [2,2]
--
-- >>> pure ($1) <*> [(+1), (*2)]
-- [2,2]
--

-- >>> Just (+3) <*> pure 1
-- Just 4
--
-- >>> pure ($ 1) <*> Just (+3)
-- Just 4
--
