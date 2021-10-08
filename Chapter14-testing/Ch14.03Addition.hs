module Addition where

-- Ch14.3 Conventional Testing - Truth according to Hspec
import Test.Hspec
import Test.QuickCheck

-- Ch14.3 Conventional Testing - Truth according to Hspec
-- sayHello :: IO ()
-- sayHello = putStrLn "hello!"


-- Ch14.3 Conventional Testing - Our First Hspec test
dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n   d count
         | n < d = (count, n)
         | otherwise =
             go (n - d) d (count + 1)

--CH14.3 Intermission Check  - pluggin from Dwayne
myMult :: (Eq a, Num a) => a -> a -> a
myMult x y
  | sy == 0 || sy == 1                 = go x y 0
  | sy == (-1) && (sx == 0 || sx == 1) = go y x 0
  | otherwise                          = go ax ay 0
  where
    sx = signum x
    sy = signum y
    ax = abs x
    ay = abs y
    go x 0 xy = xy
    go x y xy = go x (y - 1) (x + xy)

main :: IO ()
main = hspec $ do
  describe "Addition" $ do
    it "1 + 1 is greater than 1" $ do
      (1 + 1) > 1 `shouldBe` True
    it "2 + 2 is equal to 4" $ do
      2 + 2 `shouldBe` 4

--Ch14.3 Conventional testing
  describe "Division" $ do   
    it "15 divided by 3 is 5" $ do
      dividedBy 15 3 `shouldBe` (5, 0)
    it "22 divided by 5 is\
      \ 4 remainder 2" $ do
      dividedBy 22 5 `shouldBe` (4, 2)

  describe "Multiplication" $ do  
    it "6 * 3 is 18" $ do
      myMult 6 3 `shouldBe` 18
    it "3 * 6 is 18" $ do
      myMult 3 6 `shouldBe` 18
    it "6 * 0 is 0" $ do
      myMult 6 0 `shouldBe` 0
    it "0 * 6 is 0" $ do
      myMult 0 6 `shouldBe` 0
    it "6 * -3 is -18" $ do
      myMult 6 (-3) `shouldBe` (-18)
    it "-3 * 6 is -18" $ do
      myMult (-3) 6 `shouldBe` (-18)
    it "-6 * -3 is 18" $ do
      myMult (-6) (-3) `shouldBe` 18
    it "-3 * -6 is 18" $ do
      myMult (-3) (-6) `shouldBe` 18

      -- Ch14.4 QuckCheck
    it "x + 1 is always\
      \ greater than x"$ do  
        property $ \x -> x + 1 > (x :: Int)

 -- Ch14.4 QuckCheck
trivialInt :: Gen Int
trivialInt = return 1

-- return:: Monad m =>a -> m a
-- -- when m is Gen:
-- return :: a -> Gen a

--  sample (arbitrary :: Gen Double) to get some arbitary sample 
-- sample' to generate a sample list
-- >>>  sample' trivialInt 
-- [1,1,1,1,1,1,1,1,1,1,1]


oneThroughThree :: Gen Int
oneThroughThree = elements [1, 2, 3]
-- >>> sample' oneThroughThree
-- [3,2,1,3,2,1,2,3,2,3,3]

genBool :: Gen Bool
genBool = choose (False, True)
-- *Addition> sample' genBool
-- [False,True,True,False,False,False,False,True,False,False,True]

genBool' :: Gen Bool
genBool' = elements [False, True]
-- *Addition> sample' genBool'
-- [False,True,True,True,True,True,False,False,True,False,False]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]
-- *Addition> sample' genOrdering
-- [LT,GT,LT,EQ,GT,LT,GT,EQ,EQ,EQ,EQ]

genChar :: Gen Char
genChar = elements ['a'..'z']
-- *Addition> sample' genChar
-- "gxngwlbyylc"

--ARBITRARY types
genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
            a <- arbitrary
            b <- arbitrary
            return (a, b)
-- unspecified = polymorphic a and b            
-- *Addition> sample' genTuple
-- [((),()),((),()),((),()),((),()),((),()),((),()),((),()),((),()),((),()),((),()),((),())]

-- specify the type of a and b
-- *Addition> type G = Gen (Int, Float)
-- *Addition> sample (genTuple :: G)
-- (0,0.0)
-- (0,-0.46489614)
-- (2,0.3524025)
-- (-6,-4.6245003)
-- (-7,5.3642573)
-- (3,-1.9935265)
-- (-7,-7.7608557)
-- (-1,-0.74446034)
-- (12,5.6594906)
-- (11,10.288947)
-- (7,13.942571)

-- specified list and character for the Arbitrary type class
-- *Addition>  type G = Gen ([()], Char)
-- *Addition> sample (genTuple :: G)
-- ([],'2')
-- ([],',')
-- ([(),(),()],'r')
-- ([(),()],'\797085')
-- ([(),(),(),()],'!')
-- ([(),(),(),(),(),(),(),(),(),()],'$')
-- ([],'\37299')
-- ([(),(),(),(),(),(),(),(),(),(),()],'\413469')
-- ([(),(),(),()],'\1055002')
-- ([(),(),(),(),(),(),(),(),(),(),(),(),()],'\1053907')
-- ([(),()],'\EOT')

genThreeple :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (a, b, c)
genThreeple = do
            a <- arbitrary
            b <- arbitrary
            c <- arbitrary
            return (a, b, c)
--[((),(),()),((),(),()),((),(),()),((),(),()),((),(),()),((),(),()),((),(),()),((),(),()),((),(),()),((),(),()),((),(),())]

--EITHER and MAYBE

genEither :: (Arbitrary a, Arbitrary b)=> Gen (Either a b)
genEither = do
            a <- arbitrary
            b <- arbitrary
            elements [Left a, Right b]
-- >>> sample' genEither
-- [Left (),Left (),Right (),Left (),Left (),Right (),Right (),Left (),Right (),Right (),Right ()]

--Equal probability 
genMaybe :: Arbitrary a => Gen (Maybe a)
genMaybe = do
            a <- arbitrary
            elements [Nothing, Just a]
-- *Addition> sample' genMaybe
-- [Nothing,Just (),Just (),Nothing,Nothing,Just (),Nothing,Just (),Just (),Nothing,Nothing]


-- - you get more Just values
genMaybe' :: Arbitrary a => Gen (Maybe a)
genMaybe' = do
            a <- arbitrary
            frequency [ (1, return Nothing)
                      , (10, return (Just a))]

          -- - frequency :: [(Int, Gen a)] -> Gen a

-- *Addition> sample' genMaybe
-- for 3, return (Just a)
-- [Just (),Nothing,Just (),Nothing,Just (),Nothing,Nothing,Just (),Just (),Just (),Just ()]
-- for 10, return (Just a)
-- [Just (),Nothing,Just (),Nothing,Just (),Nothing,Nothing,Just (),Just (),Just (),Just ()]

-- USING QuickCheck WITHOUT Hspec
prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x

runQc :: IO ()
runQc = quickCheck prop_additionGreater


-- *Addition> runQc
-- +++ OK, passed 100 tests.


--Test for untrue
prop_additionGreater' :: Int -> Bool
prop_additionGreater' x = x + 0 > x

runQc' :: IO ()
runQc' = quickCheck prop_additionGreater'
-- *Addition> runQc'
-- *** Failed! Falsified (after 1 test):  
-- 0