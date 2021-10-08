module Instances where
-- write foldable instances for the following datatypes

-- foldMap :: Monoid m => (a -> m) -> f a -> m
-- Q1
data Constant a b = Constant b deriving Show
instance Foldable (Constant a) where
    foldMap f (Constant b) = f b


-- Q2
data Two a b = Two a b deriving Show
instance Foldable (Two a) where
    foldMap f (Two _ b) = f b
    -- foldr f i (Two _ b) = f b i 

-- Q3
data Three a b c = Three a b c deriving Show
instance Foldable (Three a b) where
    -- foldMap f (Three a b c) = f c
     foldMap f (Three _ _ c) = f c

-- Q4
data Three' a b = Three' a b b deriving Show
instance Foldable (Three' a ) where
    foldMap f (Three' _ b b') = (f b) <> (f b')

-- Q5
data Four' a b = Four' a b b b deriving Show
instance Foldable (Four' a) where
    foldMap f (Four' _ b b' b'') = (f b) <> (f b') <> (f b'')


-- End 
filterF :: ( Applicative f
           , Foldable t
           , Monoid (f a))
           => (a -> Bool) -> t a -> f a
filterF f = foldMap (\x -> if f x 
                                then pure x 
                           else mempty)


greaterThanFive = (>5)
oneToTen = [1..10]
test = filter greaterThanFive oneToTen == filterF greaterThanFive oneToTen
-- >>> test
-- True
