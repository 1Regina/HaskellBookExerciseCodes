
--Q8
module IsPalindrome where

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = 
    -- (reverse x == x) == True
-- OR simply 
    reverse x == x

-- To the start
p :: [Char]
p = "Papuchon"
awesome :: [[Char]]
awesome = [p, "curry", ":)"]
also :: [[Char]]
also =["Quake", "The Simons"]
allAwesome :: [[[Char]]]
allAwesome =[awesome, also]

--Q1
-- >>> :t length
-- length :: Foldable t => t a -> Int
-- length :: [[Char]]  -> Integer
--Q2a - d
-- >>> length [1, 2, 3, 4, 5]
-- 5
-- >>> length [(1, 2), (2, 3), (3, 4)]
-- 3
-- >>> allAwesome
-- [["Papuchon","curry",":)"],["Quake","The Simons"]]
-- >>> length allAwesome
-- 2
-- >>> concat allAwesome
-- ["Papuchon","curry",":)","Quake","The Simons"]
-- >>>  length $ concat allAwesome
-- 5

-- Q3
-- >>> 6/3
-- 2.0
-- >>> 6 / length [1, 2, 3]
-- <interactive>:1440:2-21: error:
--     • No instance for (Fractional Int) arising from a use of ‘/’
--     • In the expression: 6 / length [1, 2, 3]
--       In an equation for ‘it’: it = 6 / length [1, 2, 3]

-- 6 / 3 is ok. The other is not because length returns Int, and there is no (/) method with second argument an Int. That is, 6 / (3::Int) also fails (as does (6 :: Int) / 3).


--Q4. 
-- divisible :: Num -> Num
-- divisible = 6 `div` length [1,2,3]

--Q5
-- >>> :t (2+3 ==5)
-- (2+3 ==5) :: Bool

--Q6
--Q6a) nothing
--Q6b)
-- >>> x = 5
-- >>> :t (x + 3 == 5)
-- (x + 3 == 5) :: Bool

-- Q7 a - e
-- >>> length allAwesome == 2
-- True
--  >>> length [1, 'a', 3, 'b']
--  <interactive>:2064:10: error:

-- >>> length ['1', 'a', '3', 'b']
-- 4
-- >>> length allAwesome + length awesome
-- 5
-- >>> length allAwesome
-- 2
--
-- >>> (8 == 8) && ('b' < 'a')
-- False
-- >>> (8 == 8) && 9 -- ERROR bcos one is Bool vs 9 is Integer
-- <interactive>:2441:14: error:
--     • No instance for (Num Bool) arising from the literal ‘9’
--     • In the second argument of ‘(&&)’, namely ‘9’
--       In the expression: (8 == 8) && 9
--       In an equation for ‘it’: it = (8 == 8) && 9
--

-- Q9
myAbs:: Integer-> Integer
myAbs x = 
    if x > 0
        then x
    else  negate x 


-- Q10
f :: (a,b) -> (c,d) -> ((b,d),(a,c))
f q r = ((snd q, snd r), (fst q, fst r))
-- >>> f ('a','b') ('c','d')
-- (('b','d'),('a','c'))



-- Correcting Syntax
-- Q1
-- addOne :: a -> a
-- x = (+)
-- adding xs = w `x` 1
--   where w = length xs
j :: Int -> Int -> Int
j = (+)
adding :: Foldable t => t a -> Int
adding xs = w `j` 1
  where w = length xs -- xs must be list
-- >>> adding ("stringAdd")
-- 10
--

-- Q2
id :: x -> x
id x = x
-- >>> id 4 -- cannot run bcos clash with isPalindrome
-- *Main Lib> id 4
-- 4


--Q3
start :: (a,b) -> a 
start (a,b) = fst (a,b)
-- >>> start ('x',5)
-- 'x'
-- >>> start (5,'x')
-- 5


--Match
-- Q1 c) Show a => a -> String is the type of Show
-- Q2 b) Eq a =>a -> a -> Bool is the type of ==
-- Q3 a) (a, b) -> a is the type of fst
-- Q4 d) Num a => a -> a -> a is the type of +