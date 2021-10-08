module TypeInference where

-- TypeInference1
f1 :: Num a => a -> a -> a
f1 x y = x + y + 3
-- >>> f1 1 2
-- 6
--

-- TypeInference2
-- f2 :: Num a => a -> a -> a
f2 x y = x + y + 3
-- >>> f2 1 2
-- 6
--
