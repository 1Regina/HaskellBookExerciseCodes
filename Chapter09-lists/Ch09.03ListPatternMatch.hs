
-- myHead (x : _ ) = x

-- >>> :t myHead
-- myHead :: [a] -> a
--

-- >>> myHead [1,2,3]
-- 1


--  myTail (_ : xs) = xs
--  myTail [] = [] 
 -- take care of blank list case
 -- >>>:t myTail
 -- myTail :: [a] -> [a]
 --
-- myTail :: [a] -> [a]

 -- >>>myTail [1, 2, 3]
 -- [2,3]
 --
 -- >>> myTail [1..5]
 -- [2,3,4,5]
 --
 
-- ONLY can run in Prelude :{ ... :}
data Maybe a = Nothing | Just a deriving Show
safeTail :: [a] -> Maybe [a]
safeTail      []     = Nothing
safeTail ( _ : [] )  = Nothing
safeTail ( _ : xs )  = Just xs

safeHead :: [a] -> Maybe a
safeHead      []      = Nothing
-- safeHead  ( _ : [] )  = Nothing --if want to take care of [a] cases and return Nothing
safeHead  ( xs : _ )  = Just xs
