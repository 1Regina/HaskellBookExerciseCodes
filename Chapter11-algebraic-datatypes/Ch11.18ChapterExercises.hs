module AsPatterns where
import Data.Char 
--MCQ
--Q1 a
--Q2 c
--Q3 b
--Q4 c

-- >>> (5 + 3)
-- 8

-- doubleUp :: Show a => [a] -> [a]
doubleUp :: [a] -> [a]
doubleUp [] = []
doubleUp xs@(x: _ ) = x : xs

-- >>>  doubleUp []
-- >>> doubleUp [1]
-- >>> doubleUp [1, 2]
-- >>>  doubleUp [1, 2, 3]

-- As Patterns
--Q1
isSubseqOf::(Eq a)=>[a]->[a]-> Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf portion@(x:xs) (y:ys)
        |  x == y = isSubseqOf xs ys -- same word case
        | otherwise = isSubseqOf portion ys   

-- >>> isSubseqOf "blah" "blah"
-- True

-- >>>  isSubseqOf "blah" "wootblah"
-- True
--
-- >>> isSubseqOf "blah" "halbwoot"
-- False
--

-- >>>  isSubseqOf "blah" "blawhoot"
-- True
--

--Q2
capitalizeWords :: String -> [(String, String)]
capitalizeWords [] = []
capitalizeWords x  = concat ( map capitalizeWords' (words x)) where
  capitalizeWords' :: String -> [(String, String)]
  capitalizeWords' [] = []
  capitalizeWords' orig@(x:xs) = [(toUpper x : xs, orig)]


-- >>> words "Hello World"
-- ["Hello","World"]

-- >>>  capitalizeWords "hello porld"
-- [("Hello","hello"),("Porld","porld")]



-- Language Exercises

--Q1
capitalizeWord:: String-> String
capitalizeWord []= []
capitalizeWord (x:xs) = (toUpper x: xs)

-- >>> capitalizeWord "Chortle"
-- "Chortle"
-- >>> capitalizeWord "chortle"
-- "Chortle"


--Q2
capitalizeParagraph :: String -> String
capitalizeParagraph x = unwords $ go (words x) True where
  go :: [String] -> Bool -> [String]
  go [] _ = []
  go (x:xs) capWord
    | capWord   = (capitalizeWord x) : (go xs capNextWord) -- capitalise first word letter in one word para 
    | otherwise = x : (go xs capNextWord) -- continue with the rest of a sentence
    where
      capNextWord = (last x == '.')

-- >>> capitalizeParagraph  "blah. woot ha."
-- "Blah. Woot ha."


-- Hutton's Razor
-- Q1
data Expr = Lit Integer
          | Add Expr Expr


eval :: Expr -> Integer
eval (Lit i) = i
eval (Add expr1 expr2) = eval expr1 + eval expr2 
-- >>>  eval (Add (Lit 1) (Lit 9001))
-- 9002



-- Q2
printExpr :: Expr-> String
printExpr (Lit i) = show i
printExpr (Add expr1 expr2) = (printExpr expr1) ++ " + " ++ (printExpr expr2)

-- >>>  printExpr (Add (Lit 1) (Lit 9001))
-- "1 + 9001"


--Test
a1 = Add (Lit 9001) (Lit 1)
a2 = Add a1 (Lit 20001)
a3 = Add (Lit 1) a2

-- >>> printExpr a3
-- "1 + 9001 + 1 + 20001"

