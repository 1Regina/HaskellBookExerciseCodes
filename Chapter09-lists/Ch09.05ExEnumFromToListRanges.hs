eftBool :: Bool -> Bool -> [Bool]
eftBool x y
  | x > y = []
  | x == y = [x]
  | otherwise = x : (eftBool (succ x) y)

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd x y
  | x > y = []
  | x == y = [x]
  | otherwise = x : (eftOrd (succ x) y)

eftInt :: Int -> Int -> [Int]
eftInt x y
  | x > y = []
  | x == y = [x]
  | otherwise = x : (eftInt (succ x) y)

eftChar :: Char -> Char -> [Char]
eftChar x y
  | x > y = []
  | x == y = [x]
  | otherwise = x : (eftChar (succ x) y)



-- Basic function to use : 2 ways to write 
eftBasic :: (Enum a, Ord a) => a -> a -> [a]
eftBasic x y
  | x > y  = []
  | x == y = [x]
  | otherwise = (x : (eftBasic (succ x) y))


-- eftBasic' :: (Enum a, Ord a) => a -> a -> [a]
-- eftBasic' x y = go x y []
--   where go x y xs
--           | x > y   = xs
--           | x == y  = xs ++ (x : [])
--           | x < y   = go (succ x) y (xs ++ [x])