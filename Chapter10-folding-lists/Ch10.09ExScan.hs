
fibs    = 1 : scanl (+) 1 fibs
fibsN x = fibs !! x

-- >>>  fibsN 0
-- 1
-- >>>  fibsN 1
-- 1
--

 -- >>> fibsN 2
 -- 2
 -- >>> fibsN 6
 -- 13
 --
--Q1
fibs20 = take 20 fibs
-- >>> fibs20
-- [1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181,6765]
--
--Q2
fibs100 = takeWhile (<100) fibs
-- >>> fibs100
-- [1,1,2,3,5,8,13,21,34,55,89]
--Q3
factorial = scanl (*) 1 [1..]

-- >>> factorial !! 3
-- 6
--
-- >>> take 5 factorial
-- [1,1,2,6,24]
--
