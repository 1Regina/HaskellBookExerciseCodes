-- Q1d
-- Q2b
-- Q3d
-- q4b

-- Revieiwng Currying
cattyConny:: String-> String-> String
cattyConny x y = x ++ " mrow "  ++ y
-- fill in the types
flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy"haha"

--Q1
-- appedCatty "woohoo"  = cattyConny "woops" "woohoo"
--                      = "woops" ++ "mrow" ++ "woohoo"
--                      = "woops mrow wooohoo"

-- >>> appedCatty "woohoo"
-- "woops mrow woohoo"
--
--Q2
-- frappe "1" = flippy "haha" "1"
--            = flip cattyConny " haha" "1"
--            =  cattyConny "1" "haha"
--            = "1 mrow haha

-- >>> frappe "1"
-- "1 mrow haha"
--
--Q3
-- frappe (appedCatty "2") = frappe (cattyConny "woops" "2")
-- = frappe ("woops" ++ " mrow " ++ "2")
-- = frappe ("woops mrow 2")
-- = flippy "haha" "woops mrow 2"
-- = flip cattyConny  "haha" "woops mrow 2"
-- = cattyConny "woops mrow 2" "haha"
-- = "woops mrow 2" ++ " mrow " ++ "haha"
-- = "woops mrow 2 mrow haha"

-- >>> frappe (appedCatty "2") 
-- "woops mrow 2 mrow haha"
--

-- Q4 
-- appedCatty (frappe "blue") = cattyConny "woops" (flippy "haha" "blue")
--                            = cattyConny "woops" (flip cattyConny  "haha" "blue")
--                            = cattyConny "woops" (cattyConny "blue" "haha")
--                            = cattyConny "woops" ("blue" ++ " mrow " ++  "haha")
--                            = cattyConny "woops" "blue mrow haha"
--                            = "woops" ++ " mrow " ++ "blue mrow haha" 
--                            = "woops mrow blue mrow haha"
-- >>> appedCatty (frappe "blue")
-- "woops mrow blue mrow haha"
--

-- Q5 
-- cattyConny (frappe "pink") 
--            (cattyConny "green" (appedCatty "blue")) 
-- = cattyConny (frappe "pink") 
--              (cattyConny "green" (cattyConny "woops" "blue")) 
-- = cattyConny (frappe "pink") 
--              (cattyConny "green" ("woops" ++ " mrow " ++ "blue"))
-- = cattyConny (frappe "pink") 
--              (cattyConny "green" "woops mrow blue")  
-- = cattyConny (frappe "pink") 
--              ("green" ++ " mrow " ++  "woops mrow blue")
-- = cattyConny (frappe "pink") ("green mrow woops mrow blue") 
-- = cattyConny (flippy "haha" "pink")("green mrow woops mrow blue")
-- = cattyConny (flip cattyConny "haha" "pink" )("green mrow woops mrow blue")
-- = cattyConny (cattyConny "pink" "haha")("green mrow woops mrow blue")
-- = cattyConny ("pink" ++ " mrow "++ "haha")  ("green mrow woops mrow blue")
-- = cattyConny "pink mrow haha" "green mrow woops mrow blue"
-- = "pink mrow haha mrow green mrow woops mrow blue"


-- >>> cattyConny (frappe "pink") (cattyConny "green" (appedCatty "blue")) 
-- "pink mrow haha mrow green mrow woops mrow blue"
--

--Q6 
-- cattyConny (flippy "Pugs" "are") "awesome" = cattyConny (cattyConny "are" "Pugs") "awesome" 
--                                            = cattyConny "are mrow Pugs" "awesome"
--                                            = "are mrow Pugs mrow awesome"

-- >>>cattyConny (flippy "Pugs" "are") "awesome" 
-- "are mrow Pugs mrow awesome"
--
--Recursion
--Q1
-- dividedBy :: Integral a => a -> a -> (a, a)
-- dividedBy num denom = go num denom 0
--     where go n d count
--     | n < d = (count, n)
--     | otherwise =
--         go (n - d) d (count + 1)

-- data DividedResult a = Result a | DividedByZero deriving Show

-- dividedBy :: Integral a => a -> a -> DividedResult a
-- dividedBy num denom
--   | denom == 0                  = DividedByZero
--   | signum num == signum denom  = Result r
--   | otherwise                   = Result (-r)
--   where
--     r = go (abs num) (abs denom) 0
--     go n d count
--       | n < d     = count
--       | otherwise = go (n - d) d (count + 1)
    
-- >>> dividedBy 15 2
-- Result 7
--


--Q1
-- dividedBy 15 2
-- = go 15 2 0
-- =   go 13 2 1
-- =     go 11 2 2
-- =       go 9 2 3
-- =         go 7 2 4
-- =           go 5 2 5
-- =             go 3 2 6
-- =               go 1 2 7

-- with go--
sumUpTo :: (Eq a, Num a) => a -> a
sumUpTo max = go max 0 0 where
  go m carry i
    | i == (m + 1) = carry --base case to end
    | otherwise = go m (carry + i) (i + 1)

-- >>> sumUpTo 3
-- 6
--
sums :: (Eq a, Ord a, Num a) => a -> a
sums 1 = 1
sums n = if n > 1
            then n + sums (n - 1)
        else n + sums (n + 1)
      
        
-- >>> sums 3
-- 6
--
-- BEST
summin :: (Eq a, Num a) => a -> a
summin 0 = 0
summin n = n + summin (n-1) 

-- >>> summin 5
-- 15
--


--Q3 (WRONG)
mult :: (Integral a) => a -> a -> a
mult x y = go x y 0 where
  go x' remainingCount accum
    | remainingCount == 0 = accum
    | otherwise = go x' (remainingCount - 1) (accum + x')

-- >>> mult 2 3
-- <interactive>:298:2-5: error:
--     Variable not in scope: mult :: Integer -> Integer -> t
--

mult1:: (Integral a) => a -> a -> a
mult1 0 = const 0
mult1 n = \x -> n + mult1 (n - 1) x 

-- >>> mult1 2 3
-- 3
--

--Q4 
data DividedResult =
  Result Integer
  | DividedByZero
  deriving Show

xor :: Bool -> Bool -> Bool
xor True True = False
xor True False = True
xor False True = True
xor False False = False

dividedBy' :: Integer -> Integer -> DividedResult
dividedBy' num denom
  | denom == 0 = DividedByZero
  | otherwise = go num' denom' 0 where
    num' = abs(num)
    denom' = abs(denom)
    negativeResult = (num < 0) `xor` (denom < 0)
    go n d count
      | n < d = Result (if negativeResult then (-count) else count)
      | otherwise = go (n - d) d (count + 1)

-- >>> dividedBy' 10 (-2)
-- Result (-5)
--
