-- p1057
newtype Identity a = Identity a deriving(Eq,Ord,Show)

instance Functor Identity where 
    fmap f (Identity a) = Identity (f a) 
    
instance Applicative Identity where
    pure = Identity 
    (<*>) (Identity f) (Identity x) = Identity (f x)
 -- (<*>) :: Applicative f => f (a -> b) -> f a -> f b