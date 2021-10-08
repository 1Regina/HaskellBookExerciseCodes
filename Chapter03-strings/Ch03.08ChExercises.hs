module Ch3exercises where

-- Q1
-- >>> concat[[1, 2, 3], [ 4, 5, 6]]
-- [1,2,3,4,5,6]
-- >>> (++) [1, 2, 3] [  4, 5, 6]
-- [1,2,3,4,5,6]
-- >>> (++) "hello"" world"
-- "hello world"
-- >>> ["hello"++" world"]
-- ["hello world"]
-- >>> "hello" !! 4
-- 'o'
-- >>> (!!) "hello" 4
-- 'o'
-- >>> take 4 "lovely"
-- "love"
-- >>> take 3 "awesome"
-- "awe"
--

-- Q2
-- >>> concat[[1*6], [2*6], [3*6]]
-- [6,12,18]
-- >>> "rain"++drop 2 "elbow"
-- "rainbow"
-- >>> 10*head [1, 2, 3]
-- 10
-- >>> (take 3 "Julie") ++ (tail"yes")
-- "Jules"
-- >>> concat[tail [1, 2, 3], tail [4, 5, 6],tail [7, 8, 9]]
-- [2,3,5,6,8,9]
--

--Building Function 
--Q1 & Q2

-- curry :: String
-- curry = "Curry is awesome"

--a)
main :: IO ()
main = do
    putStr curry 
    putStrLn "!"
    where curry  = "curry is awesome"
    
-- edit :: String -> IO ()
-- edit x =
--     take x curry



choose :: Int -> [Char]
choose x = take x words
    where words = "curry is awesome"  

--c) 
exclude :: Int -> [Char]
exclude x = drop x words
    where words = "curry is awesome"

--b) & Q3
thirdletter :: String -> Char
thirdletter x = x !! 2 

--Q4
letterIndex :: Int -> Char
letterIndex x = "curry is awesome" !! x



