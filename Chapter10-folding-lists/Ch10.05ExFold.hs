--Fold
--Q1 foldr(*) 1 [1..5] gives 120

Prelude Debug.SimpleReflect> mapM_ print $ reduction $ foldr (*) 1 [1..5]
1 * (2 * (3 * (4 * (5 * 1))))
1 * (2 * (3 * (4 * 5)))
1 * (2 * (3 * 20))
1 * (2 * 60)
1 * 120
120

-- >>> foldr(*) 1 [1..5]
-- 120
--
-- >>> scanr (*) 1 [1..5]
-- [120,120,60,20,5,1]
--
--a) 
-- >>> flip(*) 1 [1..5]
-- <interactive>:283:2-17: error:
--     * Non type-variable argument in the constraint: Num [a]
--       (Use FlexibleContexts to permit this)
--     * When checking the inferred type
--         it :: forall a. (Num a, Num [a], Enum a) => [a]
--
--b)
Prelude Debug.SimpleReflect> mapM_ print $ reduction $ foldl (flip (*)) 1 [1..5]
5 * (4 * (3 * (2 * (1 * 1))))
5 * (4 * (3 * (2 * 1)))
5 * (4 * (3 * 2))
5 * (4 * 6)
5 * 24
120
-- >>>foldl(flip (*)) 1 [1..5]
-- 120
-- >>> scanl (flip (*)) 1 [1..5]
-- [1,1,2,6,24,120]
--
--c) 
Prelude Debug.SimpleReflect> mapM_ print $ reduction $ foldl  (*) 1 [1..5]
1 * 1 * 2 * 3 * 4 * 5
1 * 2 * 3 * 4 * 5
2 * 3 * 4 * 5
6 * 4 * 5
24 * 5
120
-- >>> foldl(*) 1 [1..5]
-- 120
--
-- >>> scanl (*) 1 [1..5]
-- [1,1,2,6,24,120]
--
-- Since (*) is commutative, (flip (*)) == (*) and so is the same as c).

--Q2 Since (*) is commutative, (flip (*)) == (*)
Prelude Debug.SimpleReflect> mapM_ print $ reduction $ foldl (flip (*)) 1 [1..3]
3 * (2 * (1 * 1))
3 * (2 * 1)
3 * 2
6
-- >>> foldl(flip (*)) 1 [1..3]
-- 6
-- >>> scanl (flip (*)) 1 [1..3]
-- [1,1,2,6]
--
-- >>> scanl (*) 1 [1..3]
-- [1,1,2,6]
--
-- foldl (flip (*)) 1 [1..3]
--   = foldl (flip (*)) ((flip (*)) 1 1) [2,3]
--   = foldl (flip (*)) ((flip (*)) ((flip (*)) 1 1) 2) [3]
--   = foldl (flip (*)) ((flip (*)) ((flip (*)) ((flip (*)) 1 1) 2) 3) []
--   = ((flip (*)) ((flip (*)) ((flip (*)) 1 1) 2) 3)
--   = ((flip (*)) ((flip (*)) ((*) 1 1) 2) 3)
--   = ((flip (*)) ((flip (*)) 1 2) 3)
--   = ((flip (*)) ((*) 2 1) 3)
--   = ((flip (*)) 2 3)
--   = (*) 3 2
--   = 6
--Q3 
--Ans: c - foldr is different than foldl because it associates to the right

--Q4
--Ans a - Folds are catamorphisms, which means they are used to reduce structure

--Q5
-- >>> foldr (++) [] [ "woot", "WOOT", "woot"]
-- "wootWOOTwoot"
--
-- >>> foldr max ' ' "fear is the little death"
-- 't'
-- >>> foldr (&&) True [False,True]
-- False
-- >>> foldr (||) True [False,True]
-- True
---- No, the zero/identity being True will always short-circuit (||) to
-- return True regardless of other arguments. Replace with False to fix:

-- >>> foldr (||) False [False, True]
-- True
--e)
-- >>> foldr ((++).show) "" [1..5]
-- "12345"

--
-- e) Given, foldl ((++) . show) "" [1..5]

foldl :: (Show a, Num b) => (a -> [Char] -> [Char]) -> [Char] -> [b] -> ?
foldl ::                    (x -> y -> x)           -> x      -> [y] -> x

-- x is either (Show a => a) or [Char], but that is not a problem since [Char] is
-- a valid specialization of (Show a => a).

-- However, y is either (Num b => b) or [Char] which leads to a type error.

-- Possible answer: foldl (flip ((++) . show)) "" [1..5]

-- f) Given, foldr const 'a' [1..5]

foldr :: Num a => (a -> Char -> a) -> Char -> [a] -> ?
foldr             (x -> y -> x)    -> x    -> [y] -> x

-- x is either (Num a => a) or Char and y is either (Num a => a) or Char so there
-- will be a type error due to const. (flip const) will work in its place.

-- Possible answer: foldr (flip const) 'a' [1..5] == 'a'
-- >>> foldr (flip const) 'a' [1..5] 
-- 'a'
--

-- >>> foldr const 0 [1..5]
-- 1

-- g) Given, foldr const 0 "tacos"

foldr :: Num a => (Char -> a -> Char) -> a -> [Char] -> ?
foldr ::          (x -> y -> x)       -> x -> [y]    -> x

-- x is either (Num a => a) or Char so there will be a type error due to const.
-- (flip const) will work in its place.

-- Possible answer: foldr (flip const) 0 "tacos" == 0

-- >>> foldr (flip const) 0 "tacos"
-- 0
--

-- h) Given, foldl (flip const) 0 "burritos"

foldl :: Num a => (a -> Char -> Char) -> a -> [Char] -> ?
foldl ::          (x -> y -> x)       -> x -> [y]    -> x

-- x is either (Num a => a) or Char so there will be a type error due to
-- (flip const). const will work in its place.

-- Possible answer: foldl const 0 "burritos" == 0
-- >>> foldl const 0 "burritos"
-- 0
--
-- i) Given, foldl (flip const) 'z' [1..5]

foldl :: Num a => (Char -> a -> a) -> Char -> [a] -> ?
foldl ::          (x -> y -> x)    -> x    -> [y] -> x

-- x is either (Num a => a) or Char so there will be a type error due to
-- (flip const). const will work in its place.

-- Possible answer: foldl const 'z' [1..5] == 'z'