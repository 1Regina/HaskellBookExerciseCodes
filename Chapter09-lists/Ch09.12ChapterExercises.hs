-- On Upper 
--Q1
-- >>> import Data.Char
-- >>> :t isUpper
-- isUpper :: Char -> Bool
--

-- >>> import Data.Char
-- >>> :t toUpper
-- toUpper :: Char -> Char
--
--Q2

import Data.Char
onlyUpper :: String -> String
onlyUpper str = filter isUpper str

-- >>> onlyUpper  "HbEfLrLxO"
-- "HELLO"
--
--Q3
--
capitalize :: String -> String
capitalize "" = ""
capitalize (x:xs) = (toUpper x) : xs

-- >>> import Data.Char
-- >>> capitalize "julie"
-- "Julie"
--
--Q4
allCaps :: String -> String
allCaps "" = ""
allCaps (x:xs) = (toUpper x) : allCaps xs
-- >>> import Data.Char
-- >>> allCaps "woot"
-- "WOOT"
--

--Q5
capHead :: String -> Char
-- capHead "" = ""
capHead x = toUpper (head x)
-- >>> capHead "woor"
-- 'W'



--Q6 - point free
capHead1 :: String -> Char
capHead1 = toUpper. head
-- >>> capHead1 "hoot"
-- 'H'
--

-- Writing your own standard functions Ands and Bools
-- Q1
myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs
-- >>> myOr [True, False, True]
-- True
--


myOr' :: [Bool] -> Bool
myOr' [] = False
myOr' (x:xs) = if (x == True) 
                    then True 
                 else myOr' xs
-- >>> myOr' [True, False, True]
-- True
--

--
-- Q2
myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = f x || myAny f xs
-- >>> myAny even [1, 3, 5]
-- False
--
-- >>> myAny odd [1, 3, 5]
-- True
--
-- Q3
myElem :: Eq a => a -> [a] -> Bool
myElem  _ [] = False
myElem x (y:ys) = x == y || myElem x ys

-- >>>myElem 1 [1..10]
-- True
--
myElem' :: Eq a => a -> [a] -> Bool
myElem' a = any (== a)

-- >>> myElem' 1 [1..10]
-- True
-- >>> myElem' 1 [2..10]
-- False
--
myReverse:: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]

-- >>> myReverse [1..5]
-- [5,4,3,2,1]
--
-- >>> myReverse "blah"
-- "halb"
--
--Q5
squish::[[a]]->[a]
squish [] = []
squish (x:xs) = x ++ squish xs
-- >>>squish [['a'], ['4'], ['7']]
-- "a47"
--
--Q6
squishMap::(a->[b])->[a]->[b]
squishMap f  = squish .(map f) 

 -- >>> squishMap  (\x -> "WO "++[x]++" HOO ") "123"
 -- "WO 1 HOO WO 2 HOO WO 3 HOO "
 --
 -- >>> squishMap (\x -> [1, x, 3]) [2]
 -- [1,2,3]
 --
--Q7
squishAgain::[[a]]->[a]
-- squishAgain  = squishMap id

squishAgain x = squishMap (\x -> x) x
-- >>> squishAgain [[1, 1, 3], [2,4]]
-- [1,1,3,2,4]
--

-- >>> import Data.List
-- >>> :t maximumBy
-- maximumBy :: Foldable t => (a -> a -> Ordering) -> t a -> a
--
--Q8
myMaximumBy :: (a->a-> Ordering) -> [a] -> a
myMaximumBy _ (x:[]) = x
myMaximumBy f (x:xs) = if (f x (myMaximumBy f xs)) == GT 
                           then x 
                      else (myMaximumBy f xs)
-- >>> myMaximumBy compare  [1, 53, 8001, 10]
-- 8001
--
-- >>> myMaximumBy compare [53]
-- 53

--Q9
myMinimumBy :: (a->a-> Ordering) -> [a] -> a
myMinimumBy _ (x:[]) = x
myMinimumBy f (x:xs) = if (f x (myMinimumBy f xs)) == LT 
                           then x 
                      else (myMinimumBy f xs)

-- >>> myMinimumBy compare  [8, 5, 8001, 10]
-- 5
--

myMaximum ::(Ord a) => [a] -> a
myMaximum x = myMaximumBy compare x 
-- >>> myMaximum  [8, 5, 8001, 10]
-- 8001
--

myMinimum::(Ord a) => [a] ->a
myMinimum x = myMinimumBy compare x
-- >>> myMinimum  [8, 5, 8001, 10]
-- 5
--
