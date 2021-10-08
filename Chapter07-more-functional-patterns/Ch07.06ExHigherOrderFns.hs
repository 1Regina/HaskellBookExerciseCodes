
dodgy :: Num a => a -> a -> a
oneIsOne :: Num a => a -> a
oneIsTwo :: Num a => a -> a

dodgy x y = x + y * 10
oneIsOne = dodgy 1
oneIsTwo = (flip dodgy) 2


-- >>> dodgy 1 0  -- Q1 bcos 1 + 0 * 10
-- 1
-- >>> dodgy 1 1 --Q2 bcos 1 + 1 * 10
-- 11
-- >>> dodgy 2 2 --Q3 bcos 2 + 2 * 10
-- 22
-- >>> dodgy 1 2 --Q4 bcos 1 + 2 * 10
-- 21
-- >>> dodgy 2 1 --Q5 bcos 2 + 1 * 10
-- 12
-- >>> oneIsOne 1 --Q6 bcos 1 + 1 * 10
-- 11
-- >>> oneIsOne 2 --Q7 bcos 1 + 2 * 10
-- 21
-- >>> oneIsTwo 1 --Q8 bcos (flip dodgy) 2 1 = 1 + 2 * 10 
-- 21
-- >>> oneIsTwo 2 --Q9  bcos (flip dodgy) 2 2 = 2 + 2 * 10
-- 22
-- >>> oneIsOne 3 --Q10 bcos 1 + 3 * 10
-- 31
-- >>> oneIsTwo 3 --Q11 bocs (flip dodgy) 2 3 = 3 + 2 * 10
-- 23

