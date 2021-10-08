module Stringy where

import Data.Char
import Data.List
-- Determine kinds

-- Q1
-- id :: a ->a 

-- >>> :k a
 
-- a :: *

-- Q2 
-- r ::a -> f a
-- a :: *
-- f :: * -> *

-- String Processing
-- Q1
-- data Maybe a = Nothing | Just a

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe s = Just s

-- >>>   notThe "the"

notThe' :: String -> Maybe String
notThe' x = if  x /= "the"
            then Just x
            else Nothing

-- >>> notThe' "the"


-- >>> 2 + 3
-- 5

--

-- replaceWithA :: Maybe String -> String
-- replaceWithA ( Just x) = x
-- replaceWithA Nothing = "a"

replaceThe :: String -> String
replaceThe = unwords . map (replaceWithA  . notThe) . words 
    where replaceWithA ( Just x) = x
          replaceWithA Nothing = "a"

-- Q2

data Classification = IsThe | StartsWithVowel | SomethingElse

classify :: String -> Classification
classify s
  | s == "the"        = IsThe
  | startsWithVowel s = StartsWithVowel
  | otherwise         = SomethingElse

-- isVowel :: Char -> Bool
-- isVowel c = c `elem` "aeiou"

startsWithVowel :: String -> Bool
startsWithVowel []     = False
startsWithVowel (c:_) = c `elem` "aeiou"

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = go . words  -- see https://stackoverflow.com/questions/5844653/haskell-why-the-convention-to-name-a-helper-function-go
  where
    go [] = 0
    go ("the" : wo:rd) -- where strings of "the something something"
      | startsWithVowel wo = 1 + go rd -- recursive to go on counting 1 the for "the aeiou__x"
      | otherwise = go rd -- continue with words not starting with vowel
    go (_:ws) = go ws -- continue for words in rest of sentence not paired with "the"

-- >>> countTheBeforeVowel "the cow"
-- 0
-- >>> countTheBeforeVowel "the evil cow"
-- 1

--Q3
-- check for vowels presence
isVowel :: Char -> Bool
isVowel = (`elem` "aeiou")

-- >>> isVowel 'i'
-- True
--

-- extract all the vowels
stringVowel :: String -> String
stringVowel = filter isVowel 

-- use length to find the no. of elements
countVowels :: String -> Int
countVowels  = length . stringVowel 

-- >>> countVowels "the cow"
-- 2
-- >>> countVowels "Mikolajczak"
-- 4


-- Validate the Word

countConsonant :: String -> Int
countConsonant x = length x - countVowels x
newtype Word' = Word' String deriving(Eq,Show)
vowels="aeiou"
mkWord:: String-> Maybe Word'
mkWord x = if countConsonant x >= countVowels x   
           then Just (Word' x)
           else Nothing

-- another solution
mkWord' word
  | countVowels word > countConsonant word = Nothing
  | otherwise = Just $ Word' word
    
-- Natural
data Nat = Zero | Succ Nat deriving(Eq,Show)
natToInteger:: Nat-> Integer
natToInteger Zero = 0
natToInteger (Succ n)  = succ $ natToInteger n

-- >>> natToInteger (Succ (Succ Zero))
-- 2

integerToNat:: Integer-> Maybe Nat
integerToNat x 
    | x < 0  = Nothing
    | otherwise = Just (go x)
    where 
      go y 
        | y == 0 = Zero
        | otherwise =  Succ $ go (y -1)

-- >>> integerToNat 0
-- Just Zero
-- >>> integerToNat 1
-- Just (Succ Zero)
-- >>> integerToNat 2
-- Just (Succ (Succ Zero))
--

-- Small libaray for Maybe

-- Q1
isJust:: Maybe a-> Bool
isJust Nothing = False
isJust (Just _) = True
-- >>> isJust (Just 1)
-- >>> isJust Nothing
-- True

-- False

isNothing:: Maybe a-> Bool
isNothing (Just _) = False
isNothing Nothing = True

-- >>> isNothing (Just 1)
-- >>> isNothing Nothing
-- False
-- True
--
--Q2
mayybee :: b->(a->b)-> Maybe a->b
mayybee x _ Nothing = x
mayybee _ f (Just x) = f x
-- >>> mayybee 0 (+1) Nothing
-- >>> mayybee 0 (+1) (Just 1)
-- 0
-- 2
--
--Q3
fromMaybe :: a -> Maybe a -> a
fromMaybe x Nothing = x
fromMaybe _ (Just x) = x

fromMaybe' :: a -> Maybe a -> a
fromMaybe' x whatThing = mayybee x id whatThing

-- >>> fromMaybe 0 Nothing
-- >>> fromMaybe 0 (Just 1)
-- >>> fromMaybe 5 (Just 13)
-- 0
-- 1
-- 13
-- >>> fromMaybe' 0 Nothing
-- 0
-- >>> fromMaybe 4 Nothing
-- 4

-- >>> fromMaybe' 0 (Just 1)
-- 1
-- >>> fromMaybe' 0 (Just 4)
-- 4
-- >>> fromMaybe' 4 (Just 1)
-- 1
--
-- Q4
listToMaybe ::[a]-> Maybe a 
listToMaybe (x: _) = Just x
listToMaybe []    = Nothing
-- >>>  listToMaybe [] 
-- Nothing

--
-- >>> listToMaybe [1,2,3]
-- Just 1
maybeToList:: Maybe a->[a]
maybeToList Nothing = []
maybeToList (Just x) = [x]
-- >>>  maybeToList (Just 1)
-- >>> maybeToList Nothing
-- [1]
-- []
--
--Q5
catMaybes ::[Maybe a]->[a]
catMaybes [] = []
catMaybes (Nothing : xs)  = catMaybes xs
catMaybes ((Just x) : xs) = x : (catMaybes xs)

-- >>>  catMaybes [Just 1, Nothing, Just 2]-- [1,2]

-- let xs = take 3 $ repeat Nothing ie xs = [Nothing, Nothing, Nothing]
--  >>> catMaybes xs

-- Q6 - - Refererred
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe []          = Just []
flipMaybe (Nothing:_) = Nothing
flipMaybe (Just x:xs) =
  case flipMaybe xs of
    Just ys -> Just (x:ys) -- Just [1,2,3]
    Nothing -> Nothing     -- Nothing gives Nothing

-- >>> flipMaybe [Just 1, Just 2, Just 3]
-- Just [1,2,3]
-- >>> flipMaybe [Just 1, Nothing, Just 3]
-- Nothing

-- Small Library for Either
-- Q1
-- Compare with flipMaybe
-- >>> lefts' [Left 1, Left 2, Left 3]
-- [1,2,3]
-- >>> lefts' [Left 1, Right 2, Left 3]
-- [1,3]
lefts' :: [Either a b] -> [a]
-- lefts' [] = []
-- lefts' ((Left x):xs) = x : lefts' xs
-- lefts' ((Right _):xs) = lefts' xs
lefts' = foldr f []
  where
    f (Left x) xs  = x:xs
    f (Right _) xs = xs

--Q2
rights'::[Either a b]->[b]
-- rights' [] = []
-- rights' ((Right x):xs) = x : rights' xs
-- rights' ((Left _):xs) = rights' xs
rights' = foldr f []
  where
    f (Right x) xs  = x:xs
    f (Left _) xs = xs

--Q3
partitionEithers'::[Either a b]->([a], [b])
partitionEithers' = foldr partition ([], [])
  where 
    partition (Left a) (x, xs) = (a:x, xs)
    partition (Right b) (x, xs) = (x, b: xs)

--Q4
eitherMaybe' ::(b->c)-> Either a b-> Maybe c
eitherMaybe' _ (Left _) = Nothing
eitherMaybe' f (Right x) = Just ( f x )

--Q5
either'::(a->c)->(b->c)-> Either a b->c
either' f _ (Left x) =  f x
either' _ g (Right y) =  g y

--Q6
eitherMaybe''::(b->c)-> Either a b-> Maybe c
-- eitherMaybe'' _ (Left _) = Nothing
-- eitherMaybe'' f (Right y) = Just(f y)

eitherMaybe'' f = either (const Nothing) (Just . f )


-- Iterate and unfoldr
-- 1. iternate never ends . ie must use take to get a finite list 
-- >>> :t iterate
-- iterate :: (a -> a) -> a -> [a]
-- >>> take 10 $ iterate (+1) 0
-- [0,1,2,3,4,5,6,7,8,9]
-- 2. unfoldr is more general with its Maybe
-- >>> :t unfoldr
-- unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
-- 3. So to make unfoldr do as iterate, we need to specify with its Just
-- >>> take 10 $ unfoldr (\b -> Just (b, b+1)) 0
-- [0,1,2,3,4,5,6,7,8,9]
--


mehSum :: Num a => [a] -> a
mehSum xs = go 0 xs
  where go :: Num a => a -> [a] -> a
        go n [] = n
        go n (x:xs) = go (n+x) xs

niceSum :: Num a => [a] -> a
niceSum = foldl' (+) 0

mehProduct :: Num a => [a] -> a
mehProduct xs = go 1 xs
  where go :: Num a => a -> [a] -> a
        go n [] = n
        go n (x:xs) = go (n*x) xs

niceProduct :: Num a => [a] -> a
niceProduct = foldl' (*) 1

mehConcat :: [[a]] -> [a]
mehConcat xs = go [] xs
  where go :: [a] -> [[a]] -> [a]
        go xs' [] = xs'
        go xs' (x:xs) = go (xs' ++ x) xs

myIterate::(a->a)->a->[a]
myIterate f x = x : (iterate f $ f x) 

--Q2 
myUnfoldr::(b-> Maybe(a, b))->b->[a]
myUnfoldr f b = case f b of
    Nothing       -> []
    Just (x , y)  -> x : myUnfoldr f y

--Q3
-- myUnfoldr::(b-> Maybe(a, b))->b->[a]
betterIterate::(a->a)->a->[a]
betterIterate f x = myUnfoldr g x 
 where g x = Just (x, f x)

-- another way with λ is 
betterIterate1 :: (a -> a) -> a -> [a]
betterIterate1 f x = myUnfoldr (\x' -> Just(x', (f x'))) x

-- >>>  take 10 $ iterate (+1) 0
-- >>>  take 10 $ betterIterate (+1) 0
-- [0,1,2,3,4,5,6,7,8,9]
-- [0,1,2,3,4,5,6,7,8,9]
--

-- Finally something other than a list - BINARY TREE
data BinaryTree a = Leaf
                  | Node (BinaryTree a) a (BinaryTree a)
                  deriving(Eq,Ord,Show)
-- Q1. Write unfold for BinaryTree:
unfold::(a-> Maybe(a,b,a))->a-> BinaryTree b
unfold f x = case f x of 
  Nothing         -> Leaf
  Just (p, q, r)  -> Node (unfold f p) q (unfold f r)

-- Q2 Using the unfold function you made for BinaryTree, write the following function:
treeBuild:: Integer-> BinaryTree Integer
treeBuild n 
  | n < 0 = Leaf
  | otherwise = unfold f 0  -- for the Node
    where 
      f x
       | x == n    = Nothing  
       | otherwise = Just (x+1 , x , x+1) -- Node (BinaryTree a) a (BinaryTree a)

 
        
-- >>> treeBuild 0
-- >>> treeBuild 1
-- >>> treeBuild 2
-- Leaf
-- Node Leaf 0 Leaf
-- Node (Node Leaf 1 Leaf) 0 (Node Leaf 1 Leaf)
-- >> Node (Node Leaf 1 Leaf) 
--          0 
--         (Node Leaf 1 Leaf)

-- >>> treeBuild 3
-- Node (Node (Node Leaf 2 Leaf) 1 (Node Leaf 2 Leaf)) 0 (Node (Node Leaf 2 Leaf) 1 (Node Leaf 2 Leaf))
-- Node (Node (Node Leaf 2 Leaf) 
--             1 
--            (Node Leaf 2 Leaf)) 
--       0 
--     (Node (Node Leaf 2 Leaf) 
--            1 
--           (Node Leaf 2 Leaf))