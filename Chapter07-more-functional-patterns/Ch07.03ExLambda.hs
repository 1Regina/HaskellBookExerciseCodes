--Q1
--All are equivalent because of currying

--Q2 
-- d 
-- mTh :: Num a => a -> a -> a -> a

--Q3
addOne x = x + 1
-- similar to 
addOne' = \x -> x + 1

--Q3a
f n = n + 1
-- similar to 
f' = \n -> n + 1
-- >>> f' 2
-- 3
--
--Q3b
addFive x y = ( if x > y then y else x) + 5
-- similar to 
addFive' = \x -> \y -> ( if x > y then y else x) + 5

-- >>> addFive' 3 4
-- 8

--Q3c
mflip f= \x-> \y -> f y x
-- similar to 
myflip f x y = f y x