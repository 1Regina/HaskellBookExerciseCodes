module EitherMonad where 

-- also check out https://github.com/sumidiot/haskellbook/tree/master/ch18

-- m ~ Either e

-- (>>=)  :: Monad  m => m a -> (a -> m b)        -> m b
-- (>>=)  :: Either e a      -> (a -> Either e b) -> Either e b

-- -- same as pure
-- return :: Monad m => a -> m        a 
-- return ::            a -> Either e a

type Founded = Int
type Coders  = Int

data SoftwareShop  = 
    Shop {
        founded      :: Founded, 
        programmers  :: Coders } deriving(Eq,Show)

data FoundedError  = 
      NegativeYears         Founded
    | TooManyYears          Founded
    | NegativeCoders        Coders
    | TooManyCoders         Coders
    | TooManyCodersForYears Founded Coders
    deriving(Eq,Show)

-- Functions
validateFounded
    :: Int
    -> Either FoundedError Founded

validateFounded n
    |   n  < 0    = Left $ NegativeYears n
    |   n  > 500  = Left $ TooManyYears n
    |   otherwise = Right n

validateCoders 
    :: Int 
    -> Either FoundedError Coders

validateCoders n
    |   n   < 0    = Left $ NegativeCoders n 
    |   n   > 5000 = Left $ TooManyCoders n
    |   otherwise  = Right n


mkSoftware
    :: Int
    -> Int
    -> Either FoundedError SoftwareShop

mkSoftware years coders = do
    founded     <- validateFounded years
    programmers <- validateCoders coders
    if programmers > div founded 10
        then Left  $ TooManyCodersForYears founded programmers
        else Right $ Shop founded programmers

-- Note that Either always short-circuits on the first thing to have failed because in the Monad, later values can depend on previous ones. See below:
-- >>> mkSoftware 0 0
-- Right (Shop {founded = 0, programmers = 0})

-- >>> mkSoftware (-1) 0
-- Left (NegativeYears (-1))

-- >>> mkSoftware (-1) (-1)
-- Left (NegativeYears (-1))

-- >>>  mkSoftware 0 (-1) 
-- Left (NegativeCoders (-1))

-- >>> mkSoftware 500 0
-- Right (Shop {founded = 500, programmers = 0})

-- >>> mkSoftware 501 0
-- Left (TooManyYears 501)

-- >>> mkSoftware 501 501
-- Left (TooManyYears 501)

-- >>> mkSoftware 100 5001
-- Left (TooManyCoders 5001)

-- >>> mkSoftware 0 500
-- Left (TooManyCodersForYears 0 500)


-- Conclusion:  there is no Monad for Validation. Applicative and Monad instances must have the same behavior.


-- a way of saying the Applicative apply for a type must not change behavior if derived from the Monad instance’s bind operation
import Control.Monad (ap)
(<*>) == ap

(<*>):: Applicative f 
    => f ( a -> b) -> f a -> f b
ap   :: Monad m
    => m ( a -> b) -> m a -> m b

ap   :: (Monad m)  => m (a -> b) -> m a -> m b
ap m m' = do
    x  <- m
    x' <- m'
    return (x x')
-- you can’t make a Monad for Validation that accumulates the errors like the Applicative does. Instead, any Monad instance for Validation would be identical to the Monad instance of Either


-- Exercise
data Sum a b =
        First a
    |   Second b 
    deriving(Eq,Show)

 -- (See Ch16.17ChExFunctorInstances.hs)  Q1 
instance Functor (Sum a) where
    fmap _ (First a)  = First a
    fmap f (Second b) = Second (f b)
        
-- (See Ch17.09ChapterExercises.hs) Q2
instance Applicative (Sum a) where
    pure  x = Second x
    (<*>) (First a)  _          = First a
    (<*>) _ (First a)           = First a
    (<*>) (Second f) (Second x) = Second (f x)

instance Monad (Sum a) where
    return = pure
    (First a)    >>=  _  = First a  -- (or   (>>=) (First a) _ )
    (Second b)   >>=  f  = f b 