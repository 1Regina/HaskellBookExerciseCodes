--Q1
myZip :: [a] -> [b] -> [(a, b)]
myZip _ []          = []
myZip [] _          = []
myZip (x:xs) (y:ys) = (x, y) : zip xs ys

-- >>> myZip [1, 2, 3] [4, 5, 6]
-- [(1,4),(2,5),(3,6)]
--
--Q2
myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ _ []    = []
myZipWith _ [] _    = []
myZipWith f (x:xs) (y:ys) = (f x y) : myZipWith f xs ys

-- >>> myZipWith (+) [1, 2, 3] [10, 11, 12]
-- [11,13,15]
--
-- >>> myZipWith (*) [1, 2, 3] [10, 11, 12]
-- [10,22,36]
--
-- >>>myZipWith (==) ['a'..'f'] ['a'..'m']
-- [True,True,True,True,True,True]
--
-- >>>myZipWith max [10, 5, 34, 9] [6, 8, 12, 7]
-- [10,8,34,9]
--
--Q3
myZip1 :: [a] -> [b] -> [(a, b)]
myZip1 = myZipWith (,)
-- >>> myZip1 [1, 2, 3] [4]
-- [(1,4)]
--
-- >>> myZip1 [1, 2, 3] [10, 11, 12]
-- [(1,10),(2,11),(3,12)]
--
