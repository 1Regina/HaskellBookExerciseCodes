-- Building functions Q5
module Reverse where

rvrs :: String -> String
-- rvrs x = take [1,2..16] x

-- rvrs :: [a] -> Int -> Int -> [a]
-- rvrs l i j = take (j-i+1) (drop i l)

rvrs string = last ++ middle ++ first
 where firstMiddle = take 9 string
       first = take 5 firstMiddle
       middle = drop 5 firstMiddle
       last = drop 9 string

-- Q6
main :: IO ()
-- main = print (rvrs "Curry is awesome")
-- alternatively
main = print $ rvrs "Curry is awesome"