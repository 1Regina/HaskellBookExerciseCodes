{-# LANGUAGE InstanceSigs #-}
import System.Random (StdGen, randomR)
import qualified Control.Monad.Trans.State as S (State, get, put, execState)


-- module Moi where
    
newtype Moi s a = 
    Moi { runMoi :: s -> (a , s)}

-- Implement the Functor instance for State
instance Functor (Moi s) where 
--  fmap :: (a -> b) -> f a -> f b
-- f ~ Moi s
-- g :: (s -> (a , s))
-- recall that State { runState ::           s -> (a, s) }
    fmap :: (a -> b) -> Moi s a -> Moi s b
    fmap f (Moi g) = Moi $ \s -> 
        let (x , s') = g s 
        in (f x , s')

-- Prelude> f = (+1) <$> (Moi $ \s -> (0, s))
-- Prelude> runMoi f 0
-- (1,0)


 -- Need consultation on the applicative n monad instance of Moi s a 
-- Write the Applicative instance for State:
instance Applicative (Moi s) where
    pure :: a -> Moi s a
    pure a = Moi $ \s -> (a , s)
    
    (<*>) :: Moi s (a -> b) 
          -> Moi s a
          -> Moi s b
          
  -- f :: (s -> ((a -> b), s))
  -- g :: (s -> (a, s))
    (Moi f) <*> (Moi g) = Moi $ \s -> let (x, s')  = g s
                                          (h, s'') = f s'
                                      in (h x, s'')

-- Write the Monad instance for State:                                      
instance Monad (Moi s) where
  return = pure
  -- (>>=) :: Monad m => m a -> (a -> m b) -> m b
  (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
  (Moi f) >>= g = Moi $ \s -> let (x, s') = f s 
                               in runMoi (g x) s'



-- FIZZBUZZ
-- Write a program that prints the numbers from 1 to 100. But for multiples of three print "Fizz" instead of the number and for the multiples of five print "Buzz". For numbers which are multiples of both three and five print "FizzBuzz".

fizzBuzz :: Integer -> String
fizzBuzz n | n `mod` 15 == 0 = "FizzBuzz"
           | n `mod` 5  == 0 = "Buzz"
           | n `mod` 3  == 0 = "Fizz"
           | otherwise       = show n

main1 :: IO()
main1 = mapM_ (putStrLn.fizzBuzz) [1..100]
-- mapM_ :: (Foldable t, Monad m) => (a -> m b) -> t a -> m ()

--- 
fizzbuzzList :: [Integer] -> [String]
fizzbuzzList list = S.execState (mapM_ addResult list) []

addResult :: Integer -> S.State [String] ()
addResult n = do
  xs <- S.get
  let result = fizzBuzz n
  S.put (result : xs)

main2 :: IO ()
main2 = 
    mapM_ putStrLn $ 
            reverse (fizzbuzzList [1..100])


-------
fizzbuzzListD :: [Integer] -> D.DList String
fizzbuzzListD list = S.execState (mapM_ addResultD list) D.empty

addResultD :: Integer -> S.State (D.DList String) ()
addResultD n = do
  xs <- S.get
  let result = fizzBuzz n
  S.put (D.snoc xs result)

main3 :: IO ()
main3 = mapM_ putStrLn $ fizzbuzzListD [1..100]            


----
-- FizzBuzz differently without changing the underlying data structure:
-- See fizzBuzz and addResult
-- Continue to use consing in the construction of the result list, but have it come out in the right order to begin with by enumerating the sequence backward.
fizzbuzzFromTo :: Integer -> Integer -> [String]
fizzbuzzFromTo from to = S.execState (mapM_ addResult list) []
  where list = [to, to - 1..from]

main4 :: IO ()
main4 = mapM_ putStrLn $ fizzbuzzFromTo 1 100


-- Exercoses 
-- 1. Construct a State where the state is also the value you return
get :: Moi s s
get = Moi $ \s -> (s, s)

-- >>> 2+3
