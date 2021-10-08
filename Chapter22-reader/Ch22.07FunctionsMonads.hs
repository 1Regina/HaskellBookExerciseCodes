module MonadFunction ( foo
                     , bar
                     , froot  
                     , barOne 
                     , barPlus
                     , frooty
                     , frooty'
                     ,Reader(..)) where

foo :: (Functor f, Num a) => f a -> f a
foo r = fmap (+1) r 

bar :: Foldable f => t -> f a -> (t, Int)
bar r t = (r, length t)

-- Do 2 things. Increment values and also return the length
froot :: Num a => [a] -> ([a],Int)
froot r = (map (+1) r, length r)

-- change in type signature but fst is original without increment
barOne :: Foldable t => t a -> (t a, Int)
barOne r = (r, length r)

-- so as to make fst increment
barPlus :: (Functor t, Num a, Foldable t) => t a -> (t a, Int)
barPlus r = (foo r, length r)

-- compact some more using bar so that 2 functions are just waiting for 1 argument 
frooty :: Num a => [a] -> ([a], Int)
frooty r = bar (foo r)(r)

-- Reader style with the -> 
frooty' :: Num a => [a] -> ([a] , Int)
frooty' = \r -> bar (foo r) r

-- generalise
fooBind m k = \r -> k (m r) r
-- type signature becomes
fooBind :: (t2 -> t1)       -- increment function
        -> (t1 -> t2 -> t)  -- revert the transformed function back then take length
        ->  t2              -- original list
        -> t

-- Simplify the  t types to r
fooBind' :: (r -> a)
         -> (a -> r -> b)
         -> (r -> b) -- from -> r -> b

(>>=) :: Monad m =>
     m    a   -> (a -> (m    b))  ->  m    b
    (r -> a)  -> (a -> (r -> b))  -> (r -> b)

((->) r) is our structure— the m in the type of >>=.

(>>=) :: Monad m
    => m  a -> (a  ->  m  b)  -> m  b

(>>=) ::
  (->) r a -> (a -> (->) r b) -> (->) r b
  
(>>=) ::
   (r -> a) -> (a -> r -> b) -> r -> b

return :: Monad m => a ->      m a
return ::            a -> (->) r a
return ::            a ->   r -> a

-- Applicative

(<*>) :: (r -> a -> b)
      -> (r -> a)
      -> (r -> b)

(>>=) :: (r -> a)
      -> (a -> r -> b)
      -> (r -> b)

-- Exercise


-- Q1. Implement the Reader Monad.
--  From Ch22.06
newtype Reader r a = Reader { runReader:: r -> a }

instance Functor (Reader r) where
  fmap f (Reader g) = Reader (f . g)


instance Applicative (Reader r) where
-- remember there are 2 instances pure and <*> to account

  pure :: a -> Reader r a
  -- pure a = Reader $ const a
  pure = Reader . const


-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b
  (<*>) :: Reader r (a -> b) 
        -> Reader r a 
        -> Reader r b
  (Reader rab) <*> (Reader ra) = 
      Reader $ \r -> (rab r) (ra r) -- rem Reader takes 2 arguments. see newtype Reader definition
  -- rab :: r -> (a -> b)
  -- ra :: r -> a
  -- rb :: r -> b
  -- rem applicative means 2 functions needing the same argument input and then "join" them 
  -- like this- f <*> a = \r ->f r (a r)

instance Monad (Reader r) where
  return = pure
  (>>=) :: Reader r a
        -> (a -> Reader r b)
        -> Reader r b
  (Reader r a) >>= aRb =
    Reader $ \r -> runReader (aRb (ra r)) r

  -- ra  :: r -> a
  -- rb  :: r -> b
  -- aRb :: (a -> Reader r b)
  -- aRb (ra r) :: Reader r b 
-- (>>=) :: Monad m => m a -> (a -> m b) -> m b  
-- (>>=) :: Monad         m  => m          a -> (a -> m        b) -> m         b
--          Monad (Reader r) => (Reader r) a -> (a -> Reader r b) -> (Reader r b)
  -- Rem m >>= k suggests "feed the result of computation m to the function k";


-- 2. Rewrite the monadic getDogRM to use your Reader datatype. newtype Reader r a = Reader { runReader:: r -> a }
newtype HumanName =
  HumanName String
  deriving (Eq, Show)

newtype DogName =
  DogName String
  deriving (Eq, Show)

newtype Address =
  Address String
  deriving (Eq, Show)

data Person =
  Person {
      humanName :: HumanName
    , dogName :: DogName
    , address :: Address
    } deriving (Eq, Show)

data Dog =
  Dog {
      dogsName :: DogName
    , dogsAddress :: Address
    } deriving (Eq, Show)

getDogRM :: Person -> Dog
getDogRM = do
  name <- dogName
  addy <- address
  return $ Dogname addy

getDogRM' :: Person -> Dog
-- getDogRM' = dogName >>= (\name -> address >>= \add -> return (Dog name add))
getDogRM' =
  runReader $
    Reader dogName
      >>= (\n -> Reader address
        >>= \a -> Reader (const (Dog n a)))
