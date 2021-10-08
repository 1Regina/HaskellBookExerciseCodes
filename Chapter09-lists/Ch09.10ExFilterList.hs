--Filtering
--Q1
-- >>> filter (\x -> (rem x 3) == 0) [1..30]
-- [3,6,9,12,15,18,21,24,27,30]
--
-- >>> [x | x <- [1..30] , rem x 3 == 0]
-- [3,6,9,12,15,18,21,24,27,30]
--

--Q2
-- >>> length  (filter (\x -> (rem x 3) == 0) [1..30])
-- 10
--
-- >>> length [x | x <- [1..30] , rem x 3 == 0]
-- 10
--

--Q3
myFilter :: String -> [String]
myFilter str = filter (\x -> not (elem x ["the", "a", "an"])) (words str)
 -- words is a function String -> [String]
-- >>> myFilter "the brown dog was a goof"
-- ["brown","dog","was","goof"]

myFilter' :: String -> [String]
myFilter' str =  [x | x <-  words str, not (elem x ["the", "a", "an"])]
-- >>> myFilter' "the brown dog was a goof"
-- ["brown","dog","was","goof"]
--
myFilterr :: String -> [String]
myFilterr =  filter (not . (`elem` ["the", "a", "an"])) . words
-- >>> myFilterr "the brown dog was a goof"
-- ["brown","dog","was","goof"]
--

-- >>> [x | x <- "abracadabra", elem x "aeiou"]
-- "aaaaa"
--