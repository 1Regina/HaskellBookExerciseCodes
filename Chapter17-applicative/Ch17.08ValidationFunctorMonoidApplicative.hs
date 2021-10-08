module Validity where
data Validation e a = 
      Failure e
    | Success a deriving (Eq,Show)


instance Functor (Validation a) where
  fmap f (Success a) = Success (f a) 
  fmap _ (Failure e) = Failure e 

instance Monoid a => Applicative (Validation a ) where
  pure x = Success x
  (<*>) (Success a) (Success a') = Success (a a')
  (<*>) (Success _) (Failure a') = Failure a'
  (<*>) (Failure a) (Success _) = Failure a
  (<*>) (Failure a) (Failure a') = Failure (mappend a a')