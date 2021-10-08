module RandomExample where

-- import Control.Applicative (liftA3)
-- import Control.Monad (replicateM)
-- import Control.Monad.Trans.State (State, state)
-- import System.Random (randomR, StdGen)

import System.Random
import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.Trans.State

-- Six - sided die
data Die = DieOne
         | DieTwo
         | DieThree
         | DieFour
         | DieFive
         | DieSix
         deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n = case n of
               1 -> DieOne
               2 -> DieTwo
               3 -> DieThree
               4 -> DieFour
               5 -> DieFive
               6 -> DieSix
            --    x -> error "intToDie got non 1-6 integer"


-- -- do not use error outside experiments like this
-- -- or where the branch you're ignoring is provably impossible
-- x -> error $
--     "intToDie got non 1-6 integer: "
--     ++ show x

-- randomR :: (Random a, RandomGen g) => (a, a) -> g -> (a, g)

rollDieThreeTimes :: (Die, Die, Die)
rollDieThreeTimes = do
  let s = mkStdGen 0
      (d1, s1) = randomR (1, 6) s
      (d2, s2) = randomR (1, 6) s1
      (d3, _)  = randomR (1, 6) s2
  (intToDie d1, intToDie d2, intToDie d3)


random  :: (Random a) => StdGen -> (a, StdGen)
randomR :: (...)      => (a, a) -> g -> (a, g)
-- State { runState :: s -> (a, s) }

rollDie :: State StdGen Die
rollDie = state $ do
  (n, s) <- randomR (1, 6)
  return (intToDie n, s)

-- binding the result of randomR out of the State monad the do block is in rather than using let.
rollDieBind :: State StdGen Die
rollDieBind = state $
  randomR (1, 6) >>= (\(n, s) -> return (intToDie n, s))


-- randomR :: (Random a, RandomGen g) => (a, a) -> g -> (a, g)
-- randomR :: (Int, Int) -> StdGen -> (Int, StdGen)

  -- Monad
-- (>>=) :: Monad m => m a -> (a -> m b)   -> m b
-- a ~ (Int, StdGen)
-- m ~ ((->) StdGen)
-- (>>=) :: (StdGen -> (Int, StdGen))
--       -> ((Int, StdGen) -> (StdGen -> (Die, StdGen)))
--       -> (StdGen -> (Die, StdGen))

  -- State 
-- a ~ Die
-- state :: Monad m => (s      -> (a,   s))      -> StateT s m a
-- state ::            (StdGen -> (Die, StdGen)) -> StateT StdGen m Die
-- m ~ Identity
-- state :: (StdGen -> (Die, StdGen)) -> StateT StdGen Identity Die
-- state :: (StdGen -> (Die, StdGen)) -> State StdGen Die

-- type State s          = StateT s      Identity
-- type State s      a   = StateT s      Identity a
-- type State StdGen Die = StateT StdGen Identity Die

-- We can lift intToDie over the State
-- State StdGenhas a final type argument of Int. We liftInt -> Die over it and transform that final type argument to Die.
rollDie' :: State StdGen Die
rollDie' = intToDie <$> state (randomR (1, 6))

-- randomR :: (Random a, RandomGen g) => (a, a) -> g -> (a, g)
-- randomR :: (Int, Int) -> StdGen -> (Int, StdGen)
-- randomR (1, 6) :: StdGen -> (Int, StdGen)

-- state :: Monad m => (s -> (a, s)) -> StateT s m a
-- state :: (StdGen -> (Int, StdGen)) -> StateT StdGen m Int
-- m ~ Identity
-- state :: (StdGen -> (Int, StdGen)) -> State StdGen Int


-- state f = StateT (return . f)
-- (.) :: (y -> z) -> (x -> y) -> x -> z
-- return :: Monad m => b -> m b
-- f :: StdGen -> (Int, StdGen)
-- y ~ b, z ~ m b
-- (.) :: (b -> m b) -> (x -> b) -> x -> m b
-- return (.) :: (x -> b) -> x -> m b
-- x ~ StdGen; b ~ (Int, StdGen)
-- return (.) :: (StdGen -> (Int, StdGen)) -> StdGen -> m (Int, StdGen)
-- (return . f) :: Monad m => StdGen -> m (Int, StdGen)

-- StateT :: (s ->      m (a,   s))     -> StateT s       m a
-- StateT :: (StdGen -> m (Int, StdGen)) -> StateT StdGen m Int

-- (<$>) :: Applicative f => (a -> b) -> f a -> f b
-- f ~ State StdGen, a ~ Int, b ~ Die
-- (<$>) :: (Int -> Die)
--       -> State StdGen Int
--       -> State StdGen Die

--  lift the 3-tuple constructor over three State actions that produce Die values when given an initial state to work with
rollDieThreeTimes' :: State StdGen (Die, Die, Die)
rollDieThreeTimes' = liftA3 (,,) rollDie rollDie rollDie

-- evalState :: State s a -> s -> a
-- evalState :: State StdGen (Die, Die, Die) -> StdGen -> (Die, Die, Die)

-- Prelude> evalState rollDieThreeTimes' (mkStdGen 0)
-- (DieSix,DieSix,DieFour)
-- Prelude> evalState rollDieThreeTimes' (mkStdGen 1)
-- (DieSix,DieFive,DieTwo)
-- Again, the same inputs give us the same results. What if we want a list of Die instead of a tuple?

-- knowing
-- repeat :: a -> [a]
-- fmap :: (a -> b) -> f a -> f b
-- f ~ State StdGen
-- fmap :: (Die -> [Die]) -> State StdGen Die -> State StdGen [Die]

infiniteDie :: State StdGen [Die]
infiniteDie = repeat <$> rollDie


-- ending up with 6 or n repeats value. we dont want to repeat state outcome, only want to repeat state action
-- Prelude> gen = (mkStdGen 0)
-- Prelude> take 6 $ evalState infiniteDie gen
-- [DieSix,DieSix,DieSix,DieSix,DieSix,DieSix]

-- repeat overcomes this. 
-- replicateM :: Applicative m => Int -> m a -> m [a]
-- m ~ State StdGen
replicateM :: Monad m => Int -> m a -> m [a]
-- replicateM :: Int -> State StdGen Die -> State StdGen [Die]


nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie

-- test on 5 throws. here is what we want
-- Prelude> evalState (nDie 5) (mkStdGen 0)
-- [DieSix,DieSix,DieFour,DieOne,DieFive]
-- Prelude> evalState (nDie 5) (mkStdGen 1)
-- [DieSix,DieFive,DieTwo,DieSix,DieFive]

-- 'replicateM 3' is essentially:
-- do a1 <- as
--    a2 <- as
--    a3 <- as
--    pure [a1,a2,a3]


-- KEEP ON ROLLING
-- Rolling a die until we reach or exceed a sum of 20
rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty = go 0 0
  where
    go :: Int -> Int -> StdGen -> Int
    go sum' count gen -- initial sum = 0, count = 0
      | sum' >= 20 = count -- return count if sum >=20
      | otherwise =
        -- (die, nextGen) :: (Int, StdGen)
        let (die, nextGen) = randomR (1, 6) gen
        in go (sum' + die) (count + 1) nextGen

-- randomR :: (Random a, RandomGen g) => (a, a) -> g -> (a, g)
-- randomR :: (Int, Int) -> StdGen -> (Int, StdGen)

-- Prelude> rollsToGetTwenty (mkStdGen 0)
-- 5
-- Prelude> rollsToGetTwenty (mkStdGen 0)
-- 5

-- can also use randomIO, which uses IO to get a new value each time, without needing to create a unique value for the StdGen
-- Prelude> :t randomIO
-- randomIO :: Random a => IO a
-- Prelude> rs = (rollsToGetTwenty . mkStdGen)
-- Prelude> rs <$> randomIO
-- 6
-- Prelude> rs <$> randomIO
-- 7

-- Under the hood, it’s the same interface and State Monad-driven mechanism, but it’s mutating a single, globally-used StdGen to walk the generator forward on each use.


-- Exercises
-- Q1. Refactor rollsToGetTwenty so that the limit is an argument to the function
rollsToGetN :: Int -> StdGen -> Int
rollsToGetN limit = go 0 0
  where
    go :: Int -> Int -> StdGen -> Int
    go sum' count gen -- initial sum = 0, count = 0
      | sum' >= limit = count -- return count if sum >=20
      | otherwise =
        -- (die, nextGen) :: (Int, StdGen)
        let (die, nextGen) = randomR (1, 6) gen
        in go (sum' + die) (count + 1) nextGen

-- Recall 
-- Prelude> rs = rollsToGetTwenty . mkStdGen
-- Prelude> rs <$> randomIO
-- 6
-- so.....
-- >>> (rollsToGetN 10). mkStdGen <$> randomIO 

-- Need to review again
-- Q2. Change rollsToGetN to record the series of dice that are rolled, in addition to the count of the total number of rolls:

rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged n g = (length dice, dice)
  where dice = map intToDie (rolls 0 0 g)
        rolls :: Int -> Int -> StdGen -> [Int]
        rolls dsum count gen
          | dsum >= n   = []
          | otherwise   = let (die, nextGen) = randomR (1, 6) gen
                           in die : (rolls (dsum + die) (count + 1) nextGen)