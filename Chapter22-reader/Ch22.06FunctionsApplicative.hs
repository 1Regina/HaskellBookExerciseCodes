{-# LANGUAGE InstanceSigs #-} -- for Q3
module AppliFunction ( pers
                     , chris
                     , getDog
                     , getDogR
                     , getDogR'
                     , getDogRlA2
                     ) where

import Control.Applicative(liftA2)

-- Applicative f =>
-- f ~ (->) r

-- pure :: a ->       f a
-- pure :: a -> (r -> a)

-- (<*>) ::   f (a -> b)
--         ->     f a
--         ->     f b
-- (<*>) :: (r -> a -> b)
--          ->  (r -> a)
--          ->  (r -> b)

-- Why Applicative: 
-- i) two arguments in this function, and both of them are functions waiting for the r input. When that comes, both functions will be applied to return a final result of b.

-- A) Demo applicative
-- some newtypes for tracking our different String values:
newtype HumanName =
    HumanName String deriving (Eq, Show)

newtype DogName =
    DogName String deriving (Eq, Show)

newtype Address =
    Address String deriving (Eq, Show)

data Person =
    Person {
          humanName :: HumanName
        , dogName :: DogName
        , address :: Address 
        } deriving(Eq, Show)

data Dog =
    Dog {
        dogsName   :: DogName
      , dogsAddress :: Address
      } deriving (Eq, Show)

pers :: Person
pers = Person ( HumanName "Big Bird")
              ( DogName "Barkley")
              ( Address "Sesame Street")
              
chris :: Person
chris = Person ( HumanName"Chris Allen")
               ( DogName"Papu")
               ( Address"Austin")

-- without Reader
getDog :: Person -> Dog
getDog p = Dog (dogName p) (address p)

-- 3)
-- with Reader
getDogR :: Person -> Dog
getDogR = Dog <$> dogName <*> address

(<$->>) :: (a -> b)
        -> (r -> a)
        -> (r -> b)

(<$->>) = (<$>)
(<*->>) :: (r -> a -> b)
        -> (r -> a) 
        -> (r -> b)
(<*->>) = (<*>)

-- with Reader
getDogR' :: Person -> Dog
getDogR' = Dog <$->> dogName <*->> address

-- Reader is not always Reader—sometimes it’s the ambient Applicative or Monad associated with the partially applied function type, here that is r -> .

-- 4) liftA2 is alternative to Applicative for the above manner
-- with Reader, alternative
getDogRlA2 :: Person -> Dog
getDogRlA2 = liftA2 Dog dogName address

liftA :: Applicative f => (a  -> b   -> c)
                       -> f a -> f b -> f c
-- Notice : waiting for an input from elsewhere

-- Exericse
-- Q1
myliftA2 :: Applicative f => (a  -> b   -> c)
                          -> f a -> f b -> f c
-- f == (a  -> b   -> c)
-- ra == fa
-- rb == fb
--  myliftA2 f ra rb = do the functor and applicative thing like getDogR
myliftA2 f x y = f <$> x <*> y

-- Q2
newtype Reader r a =
    Reader { runReader :: r -> a }

asks :: (r -> a) -> Reader r a
-- takes 1 argument (r -> a)  to output Reader r a 
asks f = Reader f

-- Q3. Implement the Applicative for Reader.
instance Functor (Reader r) where
--   (<$>) :: Functor f => (a -> b) -> f a -> f b
  fmap :: (a -> b) -> Reader r a -> Reader r b
-- Ch22.05 : Readers and fmap for compose
  fmap f (Reader ra) =
    -- Reader $ \r -> f (ra r)
    Reader (f . ra)


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

