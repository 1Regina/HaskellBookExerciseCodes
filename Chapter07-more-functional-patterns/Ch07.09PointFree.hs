f :: Int -> [Int] -> Int
f z xs = foldr (+) z xs

-- f = fold (+)

-- >>> f 0 [1..5]
-- 15
--
