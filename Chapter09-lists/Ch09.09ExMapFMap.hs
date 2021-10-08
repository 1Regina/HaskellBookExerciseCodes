
--Q1     take 1 $ map (+1) [undefined,2, 3]  bottom bcos cannot take (+1) to undefined
--Q2     take 1 $ map (+1) [  1, undefined, 3] returns [2]
--Q3     take 2 $ map (+1) [  1, undefined, 3] bottom bcos the 2nd in take 2 is taking an undefined
--Q4     itIsMystery xs = map (\x -> elem x "aeiou") xs  type is itIsMystery :: [Char] -> [Bool]. When applied on a list of character, it returns a list of Bool depending whether the character is a vowel

itIsMystery :: [Char] -> [Bool]
itIsMystery xs = map (\x -> elem x "aeiou") xs 
-- >>> itIsMystery ['p' , 'e' , 'i']
-- [False,True,True]
--

--Q5a
-- >>>map(^2) [  1..10]
-- [1,4,9,16,25,36,49,64,81,100]
--
--Q5b
-- >>> map minimum [[1..10], [10..20], [20..30]]
-- [1,10,20]
--
--Q5c
-- >>> map sum [[1..5], [1..5], [1..5]]
-- [15,15,15]
--
-- >>> sum[1..5]
-- 15
--

-- >>> import Data.Bool
-- >>> map (\x -> if x == 3 then (-x) else (x)) [1..10]
-- [1,2,-3,4,5,6,7,8,9,10]
--


--Q6 import Data.Bool
-- >>> import Data.Bool
-- >>> map (\x -> bool x (-x) (x == 3)) [1..10]
-- [1,2,-3,4,5,6,7,8,9,10]


--Q1
-- >>> take 1 $ map (+1) [undefined,2, 3]
-- *** Exception: Prelude.undefined
-- CallStack (from HasCallStack):
--   error, called at libraries/base/GHC/Err.hs:80:14 in base:GHC.Err

--Q2
-- >>> take 1 $ map (+1) [1, undefined,3]
-- [2]

--Q3
-- >>> take 2 $ map (+1) [1, undefined,3]
-- *** Exception: Prelude.undefined
-- CallStack (from HasCallStack):
--   error, called at libraries/base/GHC/Err.hs:80:14 in base:GHC.Err
--   undefined, called at <interactive>:208:24 in interactive:Ghci2
-- [2,
--
--Q4 it returns true or false for each aeriou in the string
-- itIsMystery xs = map (\x -> elem x "aeiou") xs
-- >>> itIsMystery "bear"
-- [False,True,True,False]
--

--Q5a-c
-- >>> map(^2) [1..10]
-- [1,4,9,16,25,36,49,64,81,100]
--
-- >>> map minimum [[1..10], [10..20], [20..30]]
-- [1,10,20]
--
-- >>> map sum [[1..5], [1..5], [1..5]]
-- [15,15,15]
--
