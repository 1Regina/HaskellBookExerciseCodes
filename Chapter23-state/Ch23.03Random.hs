import System.Random

-- Random numbers

-- System.Random is designed to generate pseudorandom values. You can generate those values by providing a seed value or by using the system-initialized generator.

-- class RandomGen g where
--   next :: g -> (Int, g)
--   genRange :: g -> (Int, Int)
--   split :: g -> (g, g)
--   {-# MINIMAL next, split #-}

-- 1.
-- newtype StdGen = StdGen { unStdGen :: SM.SMGen }
-- StdGen is a datatype that is a product of two Int32 values. These are seed values used to generate the next random number. StdGen is the standard RandomGen instance provided by the library.

-- 2.
-- mkStdGen :: Int -> StdGen
-- mkStdGen = StdGen . SM.mkSMGen . fromIntegral
-- takes an Int argument and maps it onto a generator to return a value of type StdGen, which is a pair of Int32 values.

-- 3. 
-- next :: g -> (Int, g)
-- g is a value of type StdGen. The Int that is first in the tuple is the pseudorandom number generated from the StdGen value; the second value is a new StdGen value.

-- 4.
-- random :: (RandomGen g, Random a) => g -> (a, g)
-- Similar to next but allows us to generate random values that aren't numbers. The range generated will be determined by the type.

-- demo from book as cant import due to Random (need to consult)
sg :: StdGen
sg = mkStdGen 0
-- Prelude> :t mkStdGen 0
-- mkStdGen 0 :: StdGen
-- Prelude> sg = mkStdGen 0
-- Prelude> :t next sg
-- next sg :: (Int, StdGen)
-- Prelude> next sg
-- (2147482884,40014 40692)
-- Prelude> next sg
-- (2147482884,40014 40692)

-- get the same answer twice, because the underlying function that decides which values are returned is pure—thetype doesn’t permit the performance of any effects.

-- for the next random number in sequence, use snd to extract that StdGen value and pass it as the “next” input to next:
newSg :: StdGen
newSg = snd (next sg)
-- Prelude> snd (next sg)
-- 40014 40692
-- Prelude> newSg = snd (next sg)
-- Prelude> :t newSg
-- newSg :: StdGen
-- Prelude> next newSg
-- (2092764894,1601120196 1655838864)

-- extract that StdGen value and pass it to next again to get a new tuple:
-- Prelude> next (snd (next newSg))
-- (1679949200,1635875901 2103410263)

-- random can generate values of different types, we need to specify the type to use:
-- Prelude> :t random newSg
-- random newSg :: Random a => (a, StdGen)
-- Prelude> random newSg :: (Int, StdGen)
-- (138890298504988632,439883729 1872071452)
-- Prelude> random newSg :: (Double, StdGen)
-- (0.41992072972993366,439883729 1872071452

-- For number within a range:
-- Prelude> :t randomR
-- randomR :: (RandomGen g, Random a) =>(a, a) -> g -> (a, g)
-- Prelude> r = randomR (0, 3) newSg
-- Prelude> r :: (Int, StdGen)
-- (1,1601120196 1655838864)
-- Prelude> r :: (Double, StdGen)
-- (1.259762189189801,439883729 1872071452