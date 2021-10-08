--MCQ
--Q1d
--Q2d
-- f :: Char -> String
-- g :: String -> [String]
-- g . f x = g (f x)
--Q3d
-- f :: Ord a => a -> a -> Bool
--Q4b
--Q5a

-- Let's write code
--Q1 
-- tensDigit :: Integral a => a -> a
-- tensDigit x = d
--     where xLast = x `div` 10
--           d = xLast `mod` 10

-- >>> tensDigit 49
-- 4
--
--Q1a
tensDigit :: Integral a => a -> a
tensDigit x = d
  where (xLast, _) = x `divMod` 10
        (_, d)     = xLast `divMod` 10
        -- divMod returns a tuple (multiper, remainder)
-- >>> tensDigit 49
-- 4

-- >>> tensDigit 1254
-- 5
--
--
--Q1b yes same type
--Q1c
hunsD :: Integral a => a -> a
hunsD x = d2
  where (xLast, _) = x `divMod` 100
        (_, d2)     = xLast `divMod` 100

-- >>> hunsD 253
-- 2
--
hunsD1 :: Integral a => a -> a
hunsD1 x = d
  where d = (x `div` 100) `mod` 10


-- >>> hunsD1 586
-- 5
--

-- or, pointfree/ composition
hunsD3 :: Integral a => a -> a
hunsD3 = (`mod` 10) . (`div` 100)

-- >>> hunsD3 544
-- 5
--
--Q2 (Not sure)
foldBool :: a -> a -> Bool -> a 
-- foldBool = 
--   error "Error: Need to implement foldBool!"
foldBool x y boo =
  case boo of
    True -> x
    False -> y

-- >>>  x _ False

--
foldBool2 :: a -> a -> Bool -> a
foldBool2 x y boo
  | boo == True = x
  | otherwise = y

-- >>>foldBool2 x _ False


--Q3
g :: (a -> b ) 
  -> (a , c) 
  -> (b, c)

g f (a1,c1)  = (f(a1), c1) 

