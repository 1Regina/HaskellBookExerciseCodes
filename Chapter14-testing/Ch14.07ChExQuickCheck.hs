-- 14.7

-- Validating numbers into words

-- see Ch14.07ChExNumToWords.hs

-- Using QuickCheck
module QuickCheck where
-- 1)
import Test.QuickCheck
import Data.List (sort)
import Data.Char -- for Idempotence
import Test.QuickCheck.Gen (oneof) -- Make a Gen random generator for the datatype

half x = x / 2
halfIdentity = (*2) . half

getHalf :: Double -> Bool
getHalf x = x == halfIdentity x

qc_halfIdentity :: IO ()
qc_halfIdentity = quickCheck getHalf

--  *QuickCheck> qc_halfIdentity 
-- +++ OK, passed 100 tests.

-- import Data.List (sort)
-- import Test.QuickCheck
--Q2)
listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
  snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t) = (Just y, t)
        go y (Just x, t) = (Just y, x >= y)

-- See https://wiki.haskell.org/File:Right-fold-transformation.png
--
-- listOrdered [1, 2]
-- = snd $ 1 --- go --- go
--                     /  \
--                    2   (Nothing, True)
-- = snd $ 1 --- go --- (Just 2, True)
-- = snd $ (Just 1, True)
-- = True

allOrdered :: (Ord a) => [a] -> Bool
allOrdered xs = listOrdered (sort xs)

qc_allOrdered:: IO ()
qc_allOrdered = quickCheck (allOrdered  :: [Int] -> Bool)
-- *QuickCheck> qc_allOrdered 
-- +++ OK, passed 100 tests.

--Q3
plusAssociative x y z =
  x + (y + z) == (x + y) + z

plusCommutative x y =
  x + y == y + x

prop_plusAssociative :: (Eq a, Num a) => a -> a -> a -> Bool
prop_plusAssociative x y z = plusAssociative x y z

prop_plusCommutative :: (Eq a, Num a) => a -> a -> Bool
prop_plusCommutative x y = plusCommutative x y

qc_prop_plus :: IO ()
qc_prop_plus = do
    quickCheck (prop_plusAssociative :: Int -> Int -> Int -> Bool)
    quickCheck (prop_plusCommutative :: Int -> Int -> Bool)
-- *QuickCheck> qc_prop_plus
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.

--Q4
multAssociative x y z =
  x + (y + z) == (x + y) + z

multCommutative x y =
  x + y == y + x

prop_multAssociative :: (Eq a, Num a) => a -> a -> a -> Bool
prop_multAssociative x y z = multAssociative x y z

prop_multCommutative :: (Eq a, Num a) => a -> a -> Bool
prop_multCommutative x y = multCommutative x y

qc_prop_mult :: IO ()
qc_prop_mult = do
    quickCheck (prop_multAssociative :: Int -> Int -> Int -> Bool)
    quickCheck (prop_multCommutative :: Int -> Int -> Bool)
-- Ok, one module loaded.
-- *QuickCheck> qc_prop_mult 
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.

--Q5)
-- N.B. We need to ignore division by 0 in both cases. otherwise will fail
-- *** Failed! Exception: 'divide by zero' (after 1 test):
-- 0
-- 0
prop_quotRem :: Integral a => a -> a -> Bool
-- prop_quotRem x y = (quot x y)*y + (rem x y) == x
prop_quotRem x y = y == 0 || (quot x y) * y + (rem x y) == x 

prop_divMod :: Integral a => a -> a -> Bool
-- prop_divMod x y = (div x y)*y + (mod x y) == x
prop_divMod x y = y == 0 || (div x y) * y + (mod x y) == x

qc_quotRem :: IO ()
qc_quotRem = do
  quickCheck (prop_quotRem :: Int -> Int -> Bool)


qc_divMod :: IO ()
qc_divMod = do
  quickCheck (prop_divMod :: Int -> Int -> Bool)

qc_quotRemDivMod :: IO ()
qc_quotRemDivMod = do
   quickCheck (prop_quotRem :: Int -> Int -> Bool)
   quickCheck (prop_divMod :: Int -> Int -> Bool)

-- *QuickCheck> qc_quotRemDivMod 
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.

-- Q6.
-- Is (^) associative or commutative? No. See fail results

-- Have to use Integral as Num fail
prop_powerAssociative :: (Eq a, Integral a) => a -> a -> a -> Bool
prop_powerAssociative x y z = (x ^ y) ^ z == x ^ (y ^ z)


prop_powerCommutative :: (Eq a, Integral a) => a -> a -> Bool
prop_powerCommutative x y = x ^ y == y ^ x


qc_powerAssociative :: IO ()
qc_powerAssociative = do
  quickCheck (prop_powerAssociative :: Int -> Int -> Int -> Bool)
-- It fails for x=0, y=0, z=0 since
-- LHS: (0 ^ 0) ^ 0 = 1 ^ 0 = 1
-- RHS: 0 ^ (0 ^ 0) = 0 ^ 1 = 0
-- LHS /= RHS


qc_powerCommutative :: IO ()
qc_powerCommutative = do
  quickCheck (prop_powerCommutative :: Int -> Int -> Bool)
-- It fails for x=0, y=1
-- LHS: 0 ^ 1 = 0
-- RHS: 1 ^ 0 = 1
-- LHS /= RHS

qc_prop_power :: IO ()
qc_prop_power = do
  quickCheck (prop_powerAssociative :: Int -> Int -> Int -> Bool)
  quickCheck (prop_powerCommutative :: Int -> Int -> Bool)
-- *** Failed! Falsified (after 1 test):  
-- 0
-- 0
-- 0
-- *** Failed! Falsified (after 3 tests and 3 shrinks):    
-- 0
-- 1

-- Q7
-- reverse.reverse==id

prop_reverseTwice :: String -> Bool
-- Need to limit the list to string
-- prop_reverseTwice (x:xs) = (reverse.reverse) (x:xs) == (x:xs)
-- Ok, one module loaded.
-- *QuickCheck> qc_prop_reverseTwice 
-- *** Failed! (after 1 test):                            
-- Exception:
--   Ch14.07ChExQuickCheck.hs:172:1-61: Non-exhaustive patterns in function prop_reverseTwice
prop_reverseTwice y = (reverse.reverse) y == y

qc_prop_reverseTwice :: IO ()
qc_prop_reverseTwice = do
  quickCheck (prop_reverseTwice :: String-> Bool)

-- Ok, one module loaded.
-- *QuickCheck> qc_prop_reverseTwice 
-- +++ OK, passed 100 tests.  

-- Q8
-- Write a property for the definition of $:
-- This SO answer was helpful for puzzling this one out:
-- https://stackoverflow.com/a/2017664/215168
-- prop_dollarCompose f g x = ((f . g) x) == ((\x' -> f $ g x') x)

-- prop_dollarSign f g x = ((f . g) x) == f $ g x

-- qc_prop_dollarSign = do
--   quickCheck (qc_prop_dollarSign :: Int -> Bool)

prop_dollarCompose f g x = ((f . g) x) == ((\x' -> f $ g x') x)

qc_prop_dollarCompose :: IO ()
qc_prop_dollarCompose = do
  quickCheck ((prop_dollarCompose reverse sort) ::[Integer] -> Bool)
  quickCheck ((prop_dollarCompose reverse sort) ::[Int]     -> Bool)
  quickCheck ((prop_dollarCompose reverse sort) ::[Char]    -> Bool)
  quickCheck ((prop_dollarCompose reverse sort) ::[Double]  -> Bool)
  quickCheck ((prop_dollarCompose reverse sort) ::[Float]   -> Bool)
  quickCheck ((prop_dollarCompose reverse sort) ::[Bool]    -> Bool)

-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
--Q9  See if these two functions are equal:
  --  No. foldr (:) != (++)
prop_foldrConsPlusPlus :: Eq a => [a] -> [a] -> Bool
prop_foldrConsPlusPlus x y = (foldr (:) x y) == ((++) x y)

prop_foldrPlusPlusConcat :: (Eq a, Foldable t) => t [a] -> Bool
prop_foldrPlusPlusConcat x = (foldr (++) [] x) == concat x

qc_prop_foldr :: IO ()
qc_prop_foldr = do
  quickCheck (prop_foldrConsPlusPlus :: [Integer] -> [Integer] -> Bool)
  quickCheck (prop_foldrPlusPlusConcat :: [String] -> Bool)
  quickCheck (prop_foldrPlusPlusConcat :: [[Integer]] -> Bool)
  quickCheck (prop_foldrPlusPlusConcat :: [[Int]] -> Bool)
  quickCheck (prop_foldrPlusPlusConcat :: [[Double]] -> Bool)
-- *** Failed! Falsified (after 4 tests and 6 shrinks):    
-- [0]
-- [1] fail for prop_foldrConsPlusPlus 
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.

-- Q10)
-- Nope, doesn't work for n greater than the length of the list. QuickCheck falsified it with 1 []
prop_length :: Int -> [a] -> Bool
prop_length n xs = length (take n xs) == n

qc_prop_length :: IO ()
qc_prop_length = do
    quickCheck (prop_length :: Int -> [Integer] -> Bool)
--   *** Failed! Falsified (after 3 tests and 3 shrinks):    
-- 1
-- []

-- Q11
prop_readShow :: (Eq a, Read a, Show a) => a -> Bool
prop_readShow x = (read (show x)) == x

qc_prop_readShow  :: IO ()
qc_prop_readShow  = do
      quickCheck (prop_readShow :: Integer   -> Bool)
      quickCheck (prop_readShow :: Int       -> Bool)
      quickCheck (prop_readShow :: Double    -> Bool)
      quickCheck (prop_readShow :: Char      -> Bool)
      quickCheck (prop_readShow :: String    -> Bool)
      quickCheck (prop_readShow :: [Integer] -> Bool)
      quickCheck (prop_readShow :: [Int]     -> Bool)
      quickCheck (prop_readShow :: [Double]  -> Bool)

-- *QuickCheck> qc_prop_readShow 
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.


-- Failure

-- It fails because of loss of precision when carrying out floating point operations.

square :: Num a => a -> a
square x = x * x

squareIdentity :: (Eq a, Floating a) => a -> Bool
squareIdentity x = (square . sqrt) x == id x

qc_squaresqIdentity :: IO ()
qc_squaresqIdentity  = do
      quickCheck (squareIdentity :: Double -> Bool)
      quickCheck (squareIdentity :: Float  -> Bool)

-- *** Failed! Falsified (after 2 tests and 1 shrink):     
-- 0.15
-- *** Failed! Falsified (after 3 tests):                  
-- -1.6008768

-- Idempotence
twice f = f . f
fourTimes = twice . twice

capitalizeWord :: String -> String
capitalizeWord "" = ""
capitalizeWord (x:xs) = toUpper x : xs

--Q1
f :: String -> Bool
f x =
  (capitalizeWord x
  == twice capitalizeWord x)
  &&
  (capitalizeWord x
  == fourTimes capitalizeWord x)
--Q2
f' :: Ord a => [a] -> Bool
f' x =
  (sort x
  == twice sort x)
  &&
  (sort x
  == fourTimes sort x)

qc_idempotence :: IO ()
qc_idempotence  = do
  quickCheck f
  quickCheck (f' :: [Integer] -> Bool)
  quickCheck (f' :: [Int]     -> Bool)
  quickCheck (f' :: [Double]  -> Bool)
  quickCheck (f' :: [Float]   -> Bool)
  quickCheck (f' :: [Char]    -> Bool)
  quickCheck (f' :: [Bool]    -> Bool)

--   *QuickCheck> qc_idempotence
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.

--Make a Gen random generator for the datatype
data Fool =
    Fulse
  | Frue
  deriving (Eq, Show)

genFool :: Gen Fool
genFool = do
      --  oneof [return $ Fulse, --oneof means choose either one
      --         return $ Frue]
      --another way to write
      frequency [(1, return Fulse),
                 (1, return Frue)]
-- *QuickCheck> sample genFool
-- Fulse
-- Fulse
-- Fulse
-- Frue
-- Frue
-- Frue
-- Frue
-- Fulse
-- Fulse
-- Frue
-- Frue

genFool' :: Gen Fool
genFool' = do
  frequency [(2, return Fulse),
             (1, return Frue)]
-- *QuickCheck> sample genFool'
-- Fulse
-- Fulse
-- Frue
-- Frue
-- Fulse
-- Fulse
-- Fulse
-- Frue
-- Fulse
-- Fulse
-- Fulse