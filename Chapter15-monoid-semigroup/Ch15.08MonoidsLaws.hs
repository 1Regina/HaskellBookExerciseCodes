-- module MonoidLaw where 

-- import Data.Monoid (Sum, Product)
--left identity
-- mappend :: p -> p -> p
-- mappend mempty x = x 
-- *Main Debug.SimpleReflect Data.Monoid> mappend mempty (Sum 1)
-- Sum {getSum = 1}

-- right identity
-- mappend x mempty = x
-- Prelude Debug.SimpleReflect Data.Monoid> mappend (Sum 1) mempty
-- Sum {getSum = 1}



-- associativity
 -- >>> :t (<>)
 -- (<>) :: Semigroup a => a -> a -> a

-- mappend x (mappend y z) = mappend (mappend x y) z 
-- (Sum 1) <> (Sum 2 <> Sum 3) = Sum 1 <> Sum 2) <> (Sum 3)

-- mconcat = folder mappend mempty


-- xs = [Sum 1, Sum 2, Sum 3]
-- >>> mconcat xs
-- Product {getProduct = 48}

-- foldr mappend mempty xs
-- Sum {getSum = 6}

-- >>> 2+3
-- 5

-- in Prelude import Data.Monoid
-- x = First (Just 1)
-- y = First (Just 2)
-- >>> x `mappend` y
--First {getFirst = Just 1}



-- x = Last (Just 1)
-- y = Last (Just 2)
-- >>> x `mappend` y
-- Last {getLast = Just 2}


-- class Monoid m where  
--    mempty :: m 
--    mappend :: m -> m -> m  
--    mconcat :: [m] -> m 
--    mconcat = foldr mappend mempty 