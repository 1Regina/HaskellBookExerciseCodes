--Q4 --arith4
module Arith4 where

roundTrip :: (Show a, Read a) => a -> a
-- roundTrip a = read (show a)

-- main = do
--   print (roundTrip 4)
--   print (id 4)

-- >>> roundTrip 4  
-- 4
--
-- point free


--Q5 Point Free
roundTrip = read.show 
-- >>> roundTrip 5  
-- 5
--

--Q6
roundTrip1 :: (Show a, Read b) => a -> b
roundTrip1 = read.show 

q6 = do
  print (roundTrip1 4 :: Int)
  print $ id 4

-- >>> q6
-- 4
-- 4
--
