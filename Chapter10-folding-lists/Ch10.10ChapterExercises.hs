stops = "pbtdkg"
vowels = "aeiou"
--Q1a
triples = [(x,y,z) | x <- stops, y <- vowels, z <- stops]
-- >>> triples
-- [('p','a','p'),('p','a','b'),('p','a','t'),('p','a','d'),('p','a','k'),('p','a','g'),('p','e','p'),('p','e','b'),('p','e','t'),('p','e','d'),('p','e','k'),('p','e','g'),('p','i','p'),('p','i','b'),('p','i','t'),('p','i','d'),('p','i','k'),('p','i','g'),('p','o','p'),('p','o','b'),('p','o','t'),('p','o','d'),('p','o','k'),('p','o','g'),('p','u','p'),('p','u','b'),('p','u','t'),('p','u','d'),('p','u','k'),('p','u','g'),('b','a','p'),('b','a','b'),('b','a','t'),('b','a','d'),('b','a','k'),('b','a','g'),('b','e','p'),('b','e','b'),('b','e','t'),('b','e','d'),('b','e','k'),('b','e','g'),('b','i','p'),('b','i','b'),('b','i','t'),('b','i','d'),('b','i','k'),('b','i','g'),('b','o','p'),('b','o','b'),('b','o','t'),('b','o','d'),('b','o','k'),('b','o','g'),('b','u','p'),('b','u','b'),('b','u','t'),('b','u','d'),('b','u','k'),('b','u','g'),('t','a','p'),('t','a','b'),('t','a','t'),('t','a','d'),('t','a','k'),('t','a','g'),('t','e','p'),('t','e','b'),('t','e','t'),('t','e','d'),('t','e','k'),('t','e','g'),('t','i','p'),('t','i','b'),('t','i','t'),('t','i','d'),('t','i','k'),('t','i','g'),('t','o','p'),('t','o','b'),('t','o','t'),('t','o','d'),('t','o','k'),('t','o','g'),('t','u','p'),('t','u','b'),('t','u','t'),('t','u','d'),('t','u','k'),('t','u','g'),('d','a','p'),('d','a','b'),('d','a','t'),('d','a','d'),('d','a','k'),('d','a','g'),('d','e','p'),('d','e','b'),('d','e','t'),('d','e','d'),('d','e','k'),('d','e','g'),('d','i','p'),('d','i','b'),('d','i','t'),('d','i','d'),('d','i','k'),('d','i','g'),('d','o','p'),('d','o','b'),('d','o','t'),('d','o','d'),('d','o','k'),('d','o','g'),('d','u','p'),('d','u','b'),('d','u','t'),('d','u','d'),('d','u','k'),('d','u','g'),('k','a','p'),('k','a','b'),('k','a','t'),('k','a','d'),('k','a','k'),('k','a','g'),('k','e','p'),('k','e','b'),('k','e','t'),('k','e','d'),('k','e','k'),('k','e','g'),('k','i','p'),('k','i','b'),('k','i','t'),('k','i','d'),('k','i','k'),('k','i','g'),('k','o','p'),('k','o','b'),('k','o','t'),('k','o','d'),('k','o','k'),('k','o','g'),('k','u','p'),('k','u','b'),('k','u','t'),('k','u','d'),('k','u','k'),('k','u','g'),('g','a','p'),('g','a','b'),('g','a','t'),('g','a','d'),('g','a','k'),('g','a','g'),('g','e','p'),('g','e','b'),('g','e','t'),('g','e','d'),('g','e','k'),('g','e','g'),('g','i','p'),('g','i','b'),('g','i','t'),('g','i','d'),('g','i','k'),('g','i','g'),('g','o','p'),('g','o','b'),('g','o','t'),('g','o','d'),('g','o','k'),('g','o','g'),('g','u','p'),('g','u','b'),('g','u','t'),('g','u','d'),('g','u','k'),('g','u','g')]
--Q1b
triplesP = [(x,y,z) | x <- stops, y <- vowels, z <- stops,  x == 'p']
-- >>> triplesP
-- [('p','a','p'),('p','a','b'),('p','a','t'),('p','a','d'),('p','a','k'),('p','a','g'),('p','e','p'),('p','e','b'),('p','e','t'),('p','e','d'),('p','e','k'),('p','e','g'),('p','i','p'),('p','i','b'),('p','i','t'),('p','i','d'),('p','i','k'),('p','i','g'),('p','o','p'),('p','o','b'),('p','o','t'),('p','o','d'),('p','o','k'),('p','o','g'),('p','u','p'),('p','u','b'),('p','u','t'),('p','u','d'),('p','u','k'),('p','u','g')]
--Q1c
nouns = ["ice-cream", "sunday", "boys", "family", "music"]
verbs = ["enjoy", "eat", "have", "slurp", "taste"]
triplesNVN = [(p,q,r) | p <- nouns, q <- verbs, r <- nouns]
-- >>> triplesNVN
-- [("ice-cream","enjoy","ice-cream"),("ice-cream","enjoy","sunday"),("ice-cream","enjoy","boys"),("ice-cream","enjoy","family"),("ice-cream","enjoy","music"),("ice-cream","eat","ice-cream"),("ice-cream","eat","sunday"),("ice-cream","eat","boys"),("ice-cream","eat","family"),("ice-cream","eat","music"),("ice-cream","have","ice-cream"),("ice-cream","have","sunday"),("ice-cream","have","boys"),("ice-cream","have","family"),("ice-cream","have","music"),("ice-cream","slurp","ice-cream"),("ice-cream","slurp","sunday"),("ice-cream","slurp","boys"),("ice-cream","slurp","family"),("ice-cream","slurp","music"),("ice-cream","taste","ice-cream"),("ice-cream","taste","sunday"),("ice-cream","taste","boys"),("ice-cream","taste","family"),("ice-cream","taste","music"),("sunday","enjoy","ice-cream"),("sunday","enjoy","sunday"),("sunday","enjoy","boys"),("sunday","enjoy","family"),("sunday","enjoy","music"),("sunday","eat","ice-cream"),("sunday","eat","sunday"),("sunday","eat","boys"),("sunday","eat","family"),("sunday","eat","music"),("sunday","have","ice-cream"),("sunday","have","sunday"),("sunday","have","boys"),("sunday","have","family"),("sunday","have","music"),("sunday","slurp","ice-cream"),("sunday","slurp","sunday"),("sunday","slurp","boys"),("sunday","slurp","family"),("sunday","slurp","music"),("sunday","taste","ice-cream"),("sunday","taste","sunday"),("sunday","taste","boys"),("sunday","taste","family"),("sunday","taste","music"),("boys","enjoy","ice-cream"),("boys","enjoy","sunday"),("boys","enjoy","boys"),("boys","enjoy","family"),("boys","enjoy","music"),("boys","eat","ice-cream"),("boys","eat","sunday"),("boys","eat","boys"),("boys","eat","family"),("boys","eat","music"),("boys","have","ice-cream"),("boys","have","sunday"),("boys","have","boys"),("boys","have","family"),("boys","have","music"),("boys","slurp","ice-cream"),("boys","slurp","sunday"),("boys","slurp","boys"),("boys","slurp","family"),("boys","slurp","music"),("boys","taste","ice-cream"),("boys","taste","sunday"),("boys","taste","boys"),("boys","taste","family"),("boys","taste","music"),("family","enjoy","ice-cream"),("family","enjoy","sunday"),("family","enjoy","boys"),("family","enjoy","family"),("family","enjoy","music"),("family","eat","ice-cream"),("family","eat","sunday"),("family","eat","boys"),("family","eat","family"),("family","eat","music"),("family","have","ice-cream"),("family","have","sunday"),("family","have","boys"),("family","have","family"),("family","have","music"),("family","slurp","ice-cream"),("family","slurp","sunday"),("family","slurp","boys"),("family","slurp","family"),("family","slurp","music"),("family","taste","ice-cream"),("family","taste","sunday"),("family","taste","boys"),("family","taste","family"),("family","taste","music"),("music","enjoy","ice-cream"),("music","enjoy","sunday"),("music","enjoy","boys"),("music","enjoy","family"),("music","enjoy","music"),("music","eat","ice-cream"),("music","eat","sunday"),("music","eat","boys"),("music","eat","family"),("music","eat","music"),("music","have","ice-cream"),("music","have","sunday"),("music","have","boys"),("music","have","family"),("music","have","music"),("music","slurp","ice-cream"),("music","slurp","sunday"),("music","slurp","boys"),("music","slurp","family"),("music","slurp","music"),("music","taste","ice-cream"),("music","taste","sunday"),("music","taste","boys"),("music","taste","family"),("music","taste","music")]
--
--Q2 
seekritFunc x = div (sum (map length (words x)))(length (words x))
seekritFunc:: String -> Int
-- >>> :t seekritFunc
-- seekritFunc :: String -> Int
--

-- >>> seekritFunc "i have also enjoyed this"
-- 4
--
-- >>> seekritFunc "this is lots of short words"
-- 3
--
-- Average words length


seekritFunc1 :: Fractional a => String -> a
seekritFunc1 x =
  (/) (fromIntegral (sum (map length (words x))))
      (fromIntegral (length (words x)))

-- >>> seekritFunc1 "i have also enjoyed this"
-- 4.0
-- >>> seekritFunc1 "i have also enjoyed these games"
-- 4.333333333333333
--


--Rewrite Fold Functions
--Q1
myOr::[Bool]-> Bool
myOr = foldr (||) False
-- >>> :t myOr
-- myOr :: [Bool] -> Bool
--
-- >>> myOr [False, False, False]
-- False
--
-- >>> myOr [True, False, False]
-- True
--
--Q2
myAny :: (a-> Bool) -> [a] -> Bool
myAny f  = foldr ((||) .f) False 


-- f        :: a -> Bool -- for even odd
-- (||)     :: Bool -> Bool -> Bool
-- (||) . f :: a -> Bool -> Bool -- :t (||) .even

-- >>>  myAny even [1, 3, 5]
-- False
--
--Q3
myElem :: Eq a => a -> [a] -> Bool
myElem x = any ( ==x )
-- >>> myElem 1 [1..10]
-- True
--

myElemR :: Eq a => a -> [a] -> Bool
myElemR x [] = False
myElemR x (y: ys) = x == y || myElemR x ys

-- >>> myElemR 1 [1..10]
-- True

myElemF :: Eq a => a -> [a] -> Bool
myElemF x = foldr ((||) . (== x)) False
-- >>> myElemF 1 [1..10]
-- True


--Q4
myReverse :: [a] -> [a]
-- myReverse = foldr (\a b -> b ++ [a]) []
myReverse = foldl (flip (:)) [] -- see pg 566
-- >>> myReverse "blah"
-- "halb"

--Q5
myMap :: (a->b) -> [a] -> [b]
myMap f = foldr ((:).f) []
-- f       :: a -> b
-- (:)     :: b -> [b] -> [b]
-- (:) . f :: a -> [b] -> [b]
-- alternative: myMap f = foldr (\ a b -> (f a) : b) []

-- >>> myMap (+1) [1,2,3,4]
-- [2,3,4,5]
--
--Q6
myFilter :: (a-> Bool) -> [a] -> [a]
myFilter f = foldr (\a b -> if (f a) 
                              then a:b
                            else b) 
                            []

-- >>> myFilter (>2) [1,5,3,2,1,6,4,3,2,1]  
-- [5,3,6,4,3]
--
-- >>> myFilter even [1,5,3,2,1,6,4,3,2,1]  
-- [2,6,4,2]

-- Q7

squish :: [[a]]->[a]
squish = foldr (++) []
-- >>> squish ["h", "ap" , "py"] 
-- "happy"
--
-- >>> scanr (++) []  ["h", "ap" , "py"] 
-- ["happy","appy","py",""]
--
--Q8
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f  = foldr ((++).f) []
-- >>> squishMap (\x -> [1, x, 3]) [2]
-- [1,2,3]
--
f x = "WO " ++ [x] ++ " OT "
-- >>> squishMap f "blah"
-- "WO b OT WO l OT WO a OT WO h OT "


squishMap1 :: (a -> [b]) -> [a] -> [b]
squishMap1 f = foldr (\ a b -> (f a) ++ b) []

-- >>> squishMap1 (\x -> [1, x, 3]) [2]
-- [1,2,3]

-- >>> squishMap1 f "blah"
-- "WO b OT WO l OT WO a OT WO h OT "
--
--Q9
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id
-- >>> squishAgain

-- Q10
-- It takes a comparison function and a list and returns the greatest element of
-- the list based on the last value that the comparison returned GT for
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f xs = foldr (\a b -> if f a b == GT then a else b) (last xs) xs


--  >>> myMaximumBy (\_ _ -> GT) [1..10]
--  1
--
-- >>> myMaximumBy (\_ _ -> LT) [1..10]
-- 10
-- >>> myMaximumBy compare [1..10]
-- 10
--

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f xs = foldr (\a b -> if f a b == LT then a else b) (last xs) xs

-- >>> myMinimumBy (\_ _ -> GT) [1..10]
-- 10
--
-- >>>  myMinimumBy (\_ _ -> LT) [1..10]
-- 1
--
-- >>> myMinimumBy compare [1..10]
-- 1
--
