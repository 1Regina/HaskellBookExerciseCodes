-- Examples
g x = if x + 1 == 1 
        then "AWESOME" 
      else "wut"
-- >>> g 0
-- "AWESOME"
--


funcZ x = 
    case x + 1 == 1 of
        True  -> "AWESOME"
        False -> "wut"

-- >>> funcZ 0
-- "AWESOME"
--

--Q1
functionC x y = 
    if (x > y) 
        then x 
    else y

-- >>> functionC "a" "d"
-- "d"
--
-- >>> functionC 9  3.2
-- 9.0
--

functionC' x y =
    case p of
        True  -> x
        False -> y
    where p = ( x> y) == True
-- >>> functionC' "a" "g"
-- "g"
--
-- >>> functionC' 9  3.2
-- 9.0
--
functionC'' x y =
  case x > y of
    True  -> x
    False -> y

-- >>> functionC'' "a" "u"
-- "u"
--
-- >>> functionC'' 8  3.2
-- 8.0
--

-- Q2
ifEvenAdd2 n = 
    if even n 
        then ( n + 2 ) 
    else n
-- >>> ifEvenAdd2 4
-- 6
--
-- >>> ifEvenAdd2 7
-- 7
--
ifEvenAdd2' n =
    case even n of
        True  -> (n + 2)
        False -> n

-- >>> ifEvenAdd2' 4
-- 6
--
-- >>> ifEvenAdd2' 9
-- 9
--

ifEvenAdd2'' n =
    case isEven of
        True  -> n + 2
        False -> n
    where isEven = n `rem` 2 == 0   

-- >>> ifEvenAdd2'' 7
-- 7

-- >>> ifEvenAdd2'' 14
-- 16
--

--Q3
nums :: (Ord a, Num a) => a -> a
nums x =
    case compare x 0 of 
        LT -> -1
        GT -> 1
        EQ -> 0

-- >>> nums 3 
-- 1
--
-- >>> nums 0
-- 0
--
-- >>> nums (-2)
-- -1
--