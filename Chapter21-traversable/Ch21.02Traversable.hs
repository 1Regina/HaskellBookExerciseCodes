{-# LANGUAGE ScopedTypeVariables #-}
class (Functor t, Foldable t)
    => Traversable t where
     traverse :: Applicative f
              => (a -> f b)
              -> t a
              -> f (t b)
     traverse f = sequenceA . fmap f

-- p1270
-- Travversable is stronger than Functor and Foldable. so can recover their instance for a type from Tranversable. 
-- Similaryly, Monad is stronger than Functor and Applicative so can recover them from the Monad.

-- Evaluate each action in the
-- structure from left to right,
-- and collect the results.
    
sequenceA :: Applicative f
              => t (f a)
              -> f (t a)
sequenceA = traverse id
--   {-# MINIMAL traverse | sequenceA #-}


fmap :: Functor f
    => ( a -> b)
    -> f a
    -> f b

traverse :: Applicative f
    => (a -> f b)
    -> t a
    -> f (t b)
    
myData :: [String]

myFunc :: String -> IO Record

wrong :: [IO Record]
wrong = fmap myFunc myData

right :: IO[Record]
right = traverse myFunc myData

