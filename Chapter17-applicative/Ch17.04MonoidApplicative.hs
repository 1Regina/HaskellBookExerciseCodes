import Data.Monoid
import Control.Applicative
-- See also Ch17.05
-- instance  (Monoid a, Monoid b)
--         => Monoid(a,b) where
--     mempty = (mempty, mempty)
--     (a, b) `mappend` (a',b') = (a `mappend` a', b `mappend` b')
    
-- instance Monoid a1 
--         => Applicative((,) a1) where
--     pure x = (mempty, x)
--     (u, f) <*> (v, x) = 
--      (u `mappend` v, f x)


-- [(*2), (*3)] <*> [4, 5]
-- Equals:[2*4, 2*5, 3*4, 3*5]
-- Reduced:[8,10,12,15]


-- >>> Just (*2) <*> Just 2
-- Just 4
--
-- >>> Just(*2) <*> Nothing
-- Nothing
--
-- >>> Nothing <*> Just 2
-- Nothing
--
-- >>> Nothing <*> Nothing
-- Nothing
--
-- >>>  fmap (+1) ("blah", 0)
-- ("blah",1)
--
-- >>>  ("Woo", (+1)) <*> (" Hoo!", 0)
-- ("Woo Hoo!",1)
--

-- >>> :i (,)
-- data (,) a b = (,) a b 	-- Defined in ‘GHC.Tuple’
-- instance Monoid a => Applicative ((,) a) -- Defined in ‘GHC.Base’
-- instance Functor ((,) a) -- Defined in ‘GHC.Base’
-- instance Monoid a => Monad ((,) a) -- Defined in ‘GHC.Base’
-- instance (Monoid a, Monoid b) => Monoid (a, b)
-- instance (Semigroup a, Semigroup b) => Semigroup (a, b)


-- >>> (Sum 2, (+1)) <*> (Sum 0, 0)
-- (Sum {getSum = 2},1)
-- >>>  (Product 3, (+9))<*>(Product 2, 8)
-- (Product {getProduct = 6},17)
-- >>>  (All True, (+1))<*>(All False, 0)
-- (All {getAll = False},1)

-- instance Monoid a => Monoid (Maybe a) where
--     mempty                      = Nothing
--     mappend m Nothing           = m
--     mappend Nothing m           = m
--     mappend (Just a) (Just a')  = Just (mappend a a')

-- instance Applicative Maybe where
--     pure                        = Just
--     Nothing <*> _               = Nothing
--     _       <*> Nothing         = Nothing
--     Just f  <*> Just a          = Just(f a)


-- Prelude> (,) <$> [1, 2] <*> [3, 4]
-- [(1,3),(1,4),(2,3),(2,4)]
-- You might think of it this way:
-- Prelude> (,) <$> [1, 2] <*> [3, 4]
-- fmapthe(,)over the first list:
-- [(1, ), (2, )] <*> [3, 4]
-- Then, we apply the first list to the second:
-- [(1,3),(1,4),(2,3),(2,4)]


-- >>> liftA2 (,) [1, 2] [3, 4]
-- [(1,3),(1,4),(2,3),(2,4)]
--


-- -- Control Applicative pg 1034
-- liftA  :: Applicative f =>
--           (a -> b)
--        ->  f a
--        ->  f b

-- liftA2 :: Applicative f => 
--            (a -> b -> c)
--         -> f a 
--         -> f b
--         -> f c

-- liftA3 :: Applicative f => 
--             (a -> b -> c -> d)
--         ->  f a
--         ->  f b
--         ->  f c
--         ->  f d
     -- subtly f is the list structure function [ ]    
-- >>> (+) <$> [1, 2] <*> [3, 5]
-- [4,6,5,7]
-- >>> liftA2 (+) [1, 2] [3, 5]
-- [4,6,5,7]
-- >>>  max <$> [1, 2] <*> [1,4]
-- [1,4,2,4]
-- >>> liftA2 max [1, 2] [1, 4]
-- [1,4,2,4]
--

-- p 1051
Prelude> :t lookup
lookup :: Eq a => a -> [(a, b)] -> Maybe b
Prelude> l = lookup 3 [(3, "hello")]
Prelude> l 
Just "hello"
Prelude> fmap length $ l
Just 5
Prelude> c (x:xs) = toUpper x:xs
Prelude> fmap c $ l
Just "Hello"

-- map version
Prelude> m = fromList [(3, "hello")]
Prelude> fmap c $ Data.Map.lookup 3 m
Just "Hello"