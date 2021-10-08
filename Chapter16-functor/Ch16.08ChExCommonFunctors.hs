module HeavyLift where 
import Data.Functor
-- Heavy Lifting

-- Q1
a :: Functor f => (a -> b) -> f a -> f b
a = fmap (+1) $ read "[1]" ::[Int]

-- alternatively
-- a :: [Integer]
-- a = fmap (+1) [1]
-- >>> a
-- [2]
--

-- Q2

b = (fmap.fmap) (++"lol") (Just["Hi,", "Hello"])    
-- See Ch16.08fmap.Fmap
-- >>> b
-- Just ["Hi,lol","Hellolol"]
--

-- Q3
c = fmap (*2) (\x -> x - 2 )
-- >>> c 1
-- -2
--

-- Q4
d = fmap (( return '1' ++) . show) (\x -> [x, 1..3 ] )

-- >>> d 0
-- "1[0,1,2,3]"

-- Q5. needed help
e :: IO Integer

e = let ioi = readIO "1" :: IO Integer
        changed = fmap (fmap read ("123" ++)) (fmap show ioi)  -- first fmap will make it ("123" ++) "1" so need to remove ")" so fmap again; 
    in fmap (*3) changed
-- >>> e
-- 3693
--
-- f = (*3) (read("123" ++). (show (readIO "1"))

ioing = fmap show ( readIO "1" :: IO Integer)
-- >>> ioing
-- "1"
--

changing = fmap (fmap (read ("123" ++)) fmap "3")
-- >>> changing 
-- error and need to remove the ")""

-- >>> fmap (*3) read ("123" ++ "3")
-- 3699
--

