module Traversing where
-- import Data.Traversable

traverse :: (Applicative f, Traversable t) 
         => (a -> f b) -> t a -> f (t b)
-- Compare fmap, flip bind and transverse:
-- fmap     :: (a -> b)   -> f a -> f b
-- (=<<)    :: (a -> m b) -> m a -> m b
-- traverse :: (a -> f b) -> t a -> f (t b)

-- Why Monad instance is not necessary:
-- The monadic map has a similar type signature as traverse:

-- mapM     :: (a -> m b) -> [a] -> m [b]
-- traverse :: (a -> f b) -> t a -> f (t b)
-- Indeed, mapM is traverse for the case where t is [], and f is a Monad, so traverse is a generalization that only requires Applicative, not Monad, and works for things besides list.

-- When is Traversable for
-- for: Any time you need to flip two type constructors, or map and then flip, that's probably Traversable.

-- Traverse: map a function over some embedded value(s),like fmap, but as with flip bind, that function is itself generating more structure. However, unlike with flip bind, that structure can be of a different type than the structure we lift over to apply the function. And at the end, it will flip the two structures around, as sequenceA does.

-- sequenceA :: Applicative f => t (f a) -> f (t a)
-- traverse :: Applicative f => (a -> f b) -> t a -> f (t b)

traverse f = sequenceA . fmap f
-- usually better to use traverse whenever we see asequence or sequenceA combined with a map or fmap

-- >>> fmap Just [1, 2, 3]
-- [Just 1,Just 2,Just 3]
-- >>>  sequenceA $ fmap Just [1, 2, 3]
-- Just [1,2,3]
-- >>>  sequenceA . fmap Just $ [1, 2, 3]
-- Just [1,2,3]

-- Prelude Debug.SimpleReflect> traverse Just [1, 2, 3]
-- output: Just [1,2,3]

-- Note: anytime you’re using sequenceA . fmap f, you can use traverse to achieve the same result in one step


--  Choosing Traverse or sequenceA
-- Prelude>  f = undefined :: a -> Maybe b
-- Prelude>  xs = undefined :: [a]
-- Prelude>  :t map f xs
-- map f xs :: [Maybe b]
-- usually better to use traverse whenever we see asequence or sequenceA combined with a map or fmap
-- Prelude> :t sequenceA $ map f xs
-- sequenceA $ map f xs :: Maybe [a]
-- Prelude>  :t traverse f xs
-- traverse f xs :: Maybe [b]


--p1270
-- Travversable is stronger than Functor and Foldable. so can recover their instance for a type from Tranversable. 
-- Similaryly, Monad is stronger than Functor and Applicative so can recover them from the Monad.