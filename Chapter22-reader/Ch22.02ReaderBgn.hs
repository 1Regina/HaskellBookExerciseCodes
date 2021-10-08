module AwaitingFunctions (  bip
                          , bloop
                          , bbop
                          , duwop
                          , appReader
                          , boopDoop
                          , cap
                          , rev 
                          , composed 
                          , fmapped
                          , tupled
                          , tupledMon
                          , tupledMon'
                          , tupled'
                          ) where
import Control.Applicative
import Data.Char

boop = (*2)
doop = (+10)

-- 1. functional context
-- fmap is like composition
bip :: Integer -> Integer
bip = boop . doop

-- bip and bloop are the same
bloop :: Integer -> Integer
bloop = fmap boop doop

-- 2. Applicative context
-- applicative with argument passed in parallel and results then added tog
bbop :: Integer -> Integer
bbop = (+) <$> boop <*> doop

duwop :: Integer -> Integer
duwop = liftA2 (+) boop doop

-- ((+) <$> (*2) <*> (+10)) 3
-- First the fmap
-- (*2) :: Num a => a -> a
-- (+)  :: Num a => a -> a -> a
-- (+) <$> (*2) :: Num a => a -> a -> a

-- Mapping a function awaiting two arguments over a function awaiting one produces a two argument function. and is identical to function composition:

-- (+).(*2) :: Num a => a -> a -> a

-- >>> ((+) . (*2)) 5 3
-- 13
-- >>>  ((+) <$> (*2)) 5 3
-- 13

-- First scenario Composition
-- f . g = \x -> f (g x)
-- ((+).(*2)) == \x -> (+) (2 * x)

-- The first function to get applied is(*2), and the first argument is 5.
-- (*2)takes one argument, so we get:
-- ((+).(*2)) 5 3
-- (\x->(+) (2*x))5 3
-- (\5->(+) (2*5)) 3
-- ((+) 10) 3
-- Then it adds 10 and 3
-- 13

-- Second scenario Applicative
-- ((+) <$> (*2) <*>(+10)) 3

-- ((+) <$> (*2) <*> (+10)):: Num b => b -> b
        
-- >>> :t (<*>)
-- (<*>) :: forall (f :: * -> *) a b. Applicative f => f (a -> b) -> f a -> f b
-- with f being ((->) a)
appReader :: (a -> a -> b) -> (a -> a) -> (a -> b)
appReader = (<*>)

-- we’re feeding a single argument to (*2) and (+10), and the two results form the two arguments to + :

-- ((+) <$> (*2) <*> (+10)) 3
-- (3*2) + (3+10)
-- 6 + 13
-- 19
-- use this when two functions share the same input, and we want to apply some other function to their result in order to reach a final result.


boopDoop :: Integer -> Integer
boopDoop = do
    a <- boop
    b <- doop
    return (a + b)

-- p1290 precisely the same thing as the Applicative example, but this time the context is monadic
-- Above show we can have a Functor, Applicative, and Monad for partially applied functions. In all cases, these are awaiting application to one argument that will allow both functions to be evaluated.

-- The Functor of functions is function composition. The Applicative and Monad chain the argument forward in addition to the composition (applicatives and monads are both varieties of functors, so they retain that core functorial behavior). 
-- Monadic means can also write this as do: b <- g; return $ f b

-- Reader.
-- i) It is a way of stringing functions together when all those functions are awaiting one input froma shared environment. 
-- ii) another way of abstracting out function application, and 
-- iii) it gives us a way to do computation in terms of an argumentthat hasn’t been supplied yet
-- iv) most often when we have a constant value that we will obtain from somewhere outside our program that will be an argument to a whole bunch of functions.
-- v) p1294 The Applicative and Monad instances for the function type give us a way to map a function that is awaiting an a over another function that is also awaiting an a.

-- SHORT EXERCISES. given
cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

-- to Complete
composed :: [Char] -> [Char]
composed = cap. rev

fmapped :: [Char] -> [Char]
fmapped = cap <$> rev

-- >>> composed "Julie"
-- "EILUJ"

-- >>> fmapped "Chris"
-- "SIRHC"

-- return as a tuple using Applicative
tupled ::[Char] -> ([Char], [Char])
tupled xs = (cap xs, rev xs)

-- >>> tupled "Julie"
-- ("JULIE","eiluJ")

-- Monadic style with do
tupledMon ::[Char] -> ([Char], [Char])
tupledMon = do
    a <- cap
    b <- rev
    return (a , b)

-- >>> tupledMon "Julie"
-- ("JULIE","eiluJ")

-- with >>=
-- (>>=) :: m a -> (a -> m b) -> m b 
tupledMon' ::[Char] -> ([Char], [Char])
tupledMon' = cap >>= (\x -> rev >>= (\y -> return (x , y)))

-- >>> tupledMon' "Julie"
-- ("JULIE","eiluJ")

-- pointfree style with Applicative and Functor 
tupled' :: [Char] -> ([Char], [Char])
tupled' = (,) <$> cap <*> rev
-- >>> tupled' "Julie"
-- ("JULIE","eiluJ")
