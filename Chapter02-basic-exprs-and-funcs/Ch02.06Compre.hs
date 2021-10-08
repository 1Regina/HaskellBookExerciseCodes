module Ch01triple where
triple :: Num a => a -> a
triple x1 = x1 * 3

half :: Fractional a => a -> a
half x2 = x2 / 2

square :: Num a => a -> a
square x3 = x3 * x3


piSquare :: Floating a => a -> a
piSquare x4 = pi * (x4 * x4)


x :: Integer
x = 7
y :: Integer
y = 10
f :: Integer
f = x + y

-- >>> square 5
-- 25
--

-- >>> 5 + 3
-- 8
--

-- >>> piSquare 10
-- 314.1592653589793
--
-- >>> piSquare 1
-- 3.141592653589793

