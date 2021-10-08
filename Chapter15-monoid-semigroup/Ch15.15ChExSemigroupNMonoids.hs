module SemigroupExercises where

-- import Data.Monoid () 
-- import Data.Semigroup ()
-- import Test.QuickCheck 
-- import Test.QuickCheck.Gen 
import Data.Monoid (Monoid, mappend, mempty)
import Data.Semigroup (Semigroup, Sum(..), (<>))
import Test.QuickCheck (quickCheck, oneof, Arbitrary(arbitrary), Gen, sample, CoArbitrary)
-- import Test.QuickCheck (quickCheck, oneof, Arbitrary(arbitrary), CoArbitrary)

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m, Semigroup m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m, Semigroup m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

--- Q1

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Monoid Trivial where
  mempty = Trivial
  mappend = (<>)

instance Arbitrary Trivial where
  arbitrary = return Trivial  -- See Ch14.06ArbitInstanMain.hs


type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool


main1 :: IO ()
main1 =
  quickCheck (semigroupAssoc :: TrivAssoc)


-- *SemigroupExercises> main1
-- +++ OK, passed 100 tests.
--see final main at end of file for full ideentity and asoc test
--Q2
-- Step 1: define type and data constructor
newtype Identity a = Identity a deriving (Eq, Show)


--Step 2: Semigroup instance
instance Semigroup a => Semigroup (Identity a) where
  Identity x <> Identity y = Identity (x <> y)

--Step 3:  Then Mondoid instance
instance Monoid a => Monoid (Identity a) where
  mempty = Identity mempty
  mappend = (<>) 

--Step 4 Arbitrary instance 
instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary          -- See Cj14.06ArbitInstanMain.hs under Ch14.6 Identity Crisis identityGen
    return $ Identity a


-- -- Step 5: Define type 
type IdentityTestInt = Identity [Int] -> Identity [Int] -> Identity [Int] -> Bool
type IdentityTestString = Identity [String] -> Identity [String] -> Identity [String] -> Bool

-- Step 6: quickCheck
main2 :: IO ()
main2 = do
  quickCheck (semigroupAssoc :: IdentityTestInt)
  quickCheck (semigroupAssoc :: IdentityTestString)

-- *SemigroupExercises> main2
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.

--see final main at end of file for full ideentity and asoc test
type IdentityTestInt' = Identity [Int]
type IdentityTestString'  = Identity[String]


--Q3
-- Step 1: define type and data constructor
data Two a b = Two a b deriving (Eq, Show)


--Step 2: Semigroup instance
instance (Semigroup a, Semigroup b )=> Semigroup (Two a b) where
  (Two a b ) <> (Two a' b') = Two (a <> a') (b <> b')

--Step 3:  Then Mondoid instance
instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty
  -- mappend = (<>) -- N.A for test of identity

--Step 4 Arbitrary instance 
-- twoGen :: (Arbitrary a, Arbitrary b) => Gen (Two a b)
-- twoGen = do
--   a <- arbitrary
--   b <- arbitrary
--   return $ Two a b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary          -- See Ch14.06ArbitInstanSum.hs <<sumGenFirstPls>>
    b <- arbitrary
    return $ Two a b
-- arbitrary = twoGen

-- -- Step 5: Define type 
type TwoTest = Two String [Int] -> Two String [Int] -> Two String [Int] -> Bool

-- Step 6: quickCheck
main3 :: IO ()
main3 = do
  quickCheck (semigroupAssoc :: TwoTest)

-- *SemigroupExercises> main3
-- +++ OK, passed 100 tests.

--see final main at end of file for full ideentity and asoc test
type TwoTestSI = Two String [Int]
type TwoTestIS = Two [Int] String

--Q4
-- Step 1: define type and data constructor
data Three a b c = Three a b c deriving (Eq, Show)


--Step 2: Semigroup instance
instance (Semigroup a, Semigroup b, Semigroup c )=> Semigroup (Three a b c) where
  (Three a b c ) <> (Three a' b' c') = Three (a <> a') (b <> b') (c <> c')

--Step 3:  Then Mondoid instance
instance (Monoid a, Monoid b, Monoid c) => Monoid (Three a b c) where
  mempty = Three mempty mempty mempty
  -- mappend = (<>) -- N.A for test of identity

--Step 4 Arbitrary instance 
-- threeGen :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (Three a b c)
-- tgreeGen = do
--   a <- arbitrary
--   b <- arbitrary
--   c <- arbitrary
--   return $ Three a b c

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary          -- See Ch14.06ArbitInstanSum.hs <<sumGenFirstPls>>
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c
-- arbitrary = threeGen

-- -- Step 5: Define type 
type ThreeTestSIF = Three String [Int] [Float]  -> Three String [Int] [Float]  -> Three String [Int] [Float] -> Bool
type ThreeTestISF = Three [Int] String [Float]  -> Three [Int] String [Float]  -> Three [Int] String [Float] -> Bool
type ThreeTestFSI = Three [Float] String [Int]  -> Three [Float] String [Int]  -> Three [Float] String [Int] -> Bool

-- Step 6: quickCheck
main4 :: IO ()
main4 = do
  quickCheck (semigroupAssoc :: ThreeTestSIF)
  quickCheck (semigroupAssoc :: ThreeTestISF )
  quickCheck (semigroupAssoc :: ThreeTestFSI)
  
-- *SemigroupExercises> main4
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.

--see final main at end of file for full ideentity and asoc test
type ThreeTestSIF' = Three String [Int] [Float] 
type ThreeTestISF' = Three [Int] String [Float]  
type ThreeTestFSI' = Three [Float] String [Int] 

-- Q5
-- Step 1: define type and data constructor
data Four a b c d = Four a b c d deriving (Eq, Show)


--Step 2: Semigroup instance
instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d )=> Semigroup (Four a b c d) where
  (Four a b c d) <> (Four a' b' c' d') = Four (a <> a') (b <> b') (c <> c') (d <> d')

--Step 3:  Then Mondoid instance
instance (Monoid a, Monoid b, Monoid c, Monoid d) => Monoid (Four a b c d) where
  mempty = Four mempty mempty mempty mempty
  -- mappend = (<>) -- N.A for test of identity

--Step 4 Arbitrary instance 
-- fourGen :: (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Gen (Four a b c d)
-- fourGen = do
--   a <- arbitrary
--   b <- arbitrary
--   c <- arbitrary
--   d <- arbitrary
--   return $ Four a b c d

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary          -- See Ch14.06ArbitInstanSum.hs <<sumGenFirstPls>>
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four a b c d
-- arbitrary = fourGen

-- -- Step 5: Define type 
type FourTestSIFC = Four String [Int] [Float] [Char] -> Four String [Int] [Float] [Char]  -> Four String [Int] [Float] [Char] -> Bool
type FourTestCISF = Four [Char] [Int] String [Float] -> Four [Char] [Int] String [Float]  -> Four [Char] [Int] String [Float] -> Bool
type FourTestFSCI = Four [Float] String [Char] [Int] -> Four [Float] String [Char] [Int]  -> Four [Float] String [Char] [Int] -> Bool

-- Step 6: quickCheck
main5 :: IO ()
main5 = do
  quickCheck (semigroupAssoc :: FourTestSIFC)
  quickCheck (semigroupAssoc :: FourTestCISF)
  quickCheck (semigroupAssoc :: FourTestFSCI)
  
-- *SemigroupExercises> main5
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.

--see final main at end of file for full ideentity and asoc test
type FourTestSFIE' = Four String [Float] (Sum Int) (Either Double Char)
type FourTestEISF' = Four (Either Double Char) (Sum Int) String [Float] 
type FourTestFSEI'  = Four [Float] String (Either Double Char) (Sum Int)

-- remove either
type FourTestSFIC' = Four String [Float] (Sum Int) [Char]
type FourTestCISF' = Four [Char] (Sum Int) String [Float] 
type FourTestFSCI'  = Four [Float] String [Char] (Sum Int)

-- Q6

-- Step 1: define type and data constructor
newtype BoolConj = BoolConj Bool deriving (Eq, Show)


--Step 2: Semigroup instance
instance Semigroup BoolConj where
   (BoolConj booll) <> (BoolConj booll') = BoolConj (booll && booll') 

--Step 3:  Then Mondoid instance
instance Monoid BoolConj where
  mempty = BoolConj True
  mappend = (<>) 

--Step 4 Arbitrary instance 
-- bullConj :: (Arbitrary a) => Gen (BoolConj Bool)
-- bullConj = do
--   a <- arbitrary
--   return $ BoolConj Bool

instance Arbitrary BoolConj where
  arbitrary = do
    a <- arbitrary   
    return $ BoolConj a

-- Form Semigroup test
-- *SemigroupExercises>  (BoolConj True) <> (BoolConj True)
-- BoolConj True
-- *SemigroupExercises>  (BoolConj True) <> (BoolConj False)
-- BoolConj False
-- *SemigroupExercises> 

-- For Monoid test
-- *SemigroupExercises> (BoolConj True) `mappend` mempty
-- BoolConj True
-- *SemigroupExercises> mempty `mappend` (BoolConj False)
-- BoolConj False

-- -- Step 5: Define type 
type BoolConjTest = BoolConj -> BoolConj -> BoolConj -> Bool

-- Step 6: quickCheck for type
main6 :: IO ()
main6 = do
  quickCheck (semigroupAssoc :: BoolConjTest)

-- -*SemigroupExercises> main6
-- +++ OK, passed 100 tests.
--see final main at end of file for full ideentity and asoc test


-- Q7

-- Step 1: define type and data constructor
newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)


--Step 2: Semigroup instance
instance Semigroup BoolDisj where
   (BoolDisj booll) <> (BoolDisj booll') = BoolDisj (booll || booll') 

--Step 3:  Then Mondoid instance
instance Monoid BoolDisj where
  mempty = BoolDisj True
  mappend = (<>) 

--Step 4 Arbitrary instance 
-- bullDisj :: (Arbitrary a) => Gen (BoolDisj Bool)
-- bullDisj = do
--   a <- arbitrary
--   return $ BoolDisj Bool

instance Arbitrary BoolDisj where
  arbitrary = do
    a <- arbitrary   
    return $ BoolDisj a


-- For Semigroup test match results
-- *SemigroupExercises>  (BoolDisj True) <> (BoolDisj True)
-- BoolDisj True
-- *SemigroupExercises>  (BoolDisj True) <> (BoolDisj False)
-- BoolDisj True
-- *SemigroupExercises> 

-- For Monoid test match results
-- *SemigroupExercises> (BoolDisj True) `mappend` mempty
-- BoolDisj True
-- *SemigroupExercises> mempty `mappend` (BoolDisj False)
-- BoolDisj True

-- -- Step 5: Define type 
type BoolDisjTest = BoolDisj -> BoolDisj -> BoolDisj -> Bool

-- Step 6: quickCheck for type
main7 :: IO ()
main7 = do
  quickCheck (semigroupAssoc :: BoolDisjTest)

-- *SemigroupExercises> main7
-- +++ OK, passed 100 tests.

--see final main at end of file for full ideentity and asoc test

--Q8

-- Step 1: define type and data constructor
data Or a b = Fst a | Snd b deriving (Eq, Show)


--Step 2: Semigroup instance
instance (Semigroup a, Semigroup b ) => Semigroup (Or a b) where
--   -- (Fst a) <> (Snd b) = Snd b
--   -- (Fst a) <> (Fst b) = Fst b
--   -- (Snd a) <> _ = Snd a
--   -- (Snd a) <> (Snd b) = Snd a
-- -- Summary of the 4 becomes: 
      (Snd x) <> _ = Snd x
      _       <> x = x

--Step 3:  Then Mondoid instance (NA bcos same)
-- instance (Monoid a, Monoid b) => Monoid (Or a b) where
      -- mempty                     = First (n.a)
      -- (Snd x) <> _ = Snd x
      -- _       <> x = x
--   -- mappend = (<>) -- N.A for test of identity

-- --Step 4 Arbitrary instance 
-- orGen :: (Arbitrary a, Arbitrary b) => Gen (Or a b)
-- orGen = do
--   a <- arbitrary
--   b <- arbitrary
--   return $ Or a b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    a <- arbitrary          -- See Ch14.07ChExQuickCheck <GenFool>
    b <- arbitrary
    oneof [ return $ Fst a ,
            return $ Snd b ]
    -- frequency [ (1, return a)     -- another way to write
    --           , (1, return b)« ]



-- Step 5: Define type 
type OrTest = Or String [Int] -> Or String [Int] -> Or String [Int] -> Bool

-- Step 6: quickCheck
main8 :: IO ()
main8 = do
  quickCheck (semigroupAssoc :: OrTest)

-- *SemigroupExercises> main8
-- +++ OK, passed 100 tests.

--see final main at end of file for full ideentity and asoc test
type OrTestSI' = Or [String] [Int]
type OrTestCI' = Or [Char] [Int]

-- Q9 - needed help 
-- (CoArbitrary https://stackoverflow.com/questions/47849407/coarbitrary-in-haskell) ; 
-- fmap https://stackoverflow.com/questions/13134825/how-do-functors-work-in-haskell )
-- Step 1: define type and data constructor
newtype Combine a b = Combine { unCombine :: (a -> b)}

-- Step 2: Semigroup instance -- always return b
instance (Semigroup b) => Semigroup (Combine a b) where
-- (Combine f) <> (Combine g) = Combine (\x -> (f x) <> (g x))
    Combine f <> Combine g = Combine (f <> g)
-- instance Semigroup b => Semigroup (a -> b) where
--   f <> g = \x -> f x <> g x

-- for good measures that Combine (a b) cam show . See quickcheck in mainRemainder
instance Show (Combine a b) where
  show _ = "Combine a b"

-- Step 3:  Then Mondoid instance -- always return b
-- instance (Semigroup b, Monoid b) => Monoid (Combine a b) where
instance (Monoid b) => Monoid (Combine a b) where  
  mempty = Combine mempty
  mappend = (<>)
-- Step 4 Arbitrary instance 
-- Use CoAribtrary to reduce a to same type as b, then you can apply fmap
instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = fmap Combine arbitrary

-- Step 5: Define function  
combineSemigroupAssoc :: (Eq b, Show b, Semigroup b)
                      => a
                      -> Combine a b
                      -> Combine a b
                      -> Combine a b
                      -> Bool

combineSemigroupAssoc x (Combine f) (Combine g) (Combine h) =
  (f x <> (g x <> h x)) == ((f x <> g x) <> h x)

-- Step 6: Define the type for test. See quickCheck in mainRemainder below
type CombineTest = Combine String [Int]

f = Combine $ \n -> Sum (n + 1)

g = Combine $ \n -> Sum (n - 1)

res1 :: Sum Int
res1 = unCombine (f <> g) 0 

res2 :: Sum Int
res2 = unCombine (mappend f mempty) 0

-- For Semigroup test- results match
-- Ok, one module loaded.
-- *SemigroupExercises> unCombine (f <> g) $ 0
-- Sum {getSum = 0}
-- *SemigroupExercises> unCombine (f <> g) $ 1
-- Sum {getSum = 2}
-- *SemigroupExercises> unCombine (f <> f) $ 1
-- Sum {getSum = 4}
-- *SemigroupExercises> 
-- *SemigroupExercises> unCombine (g <> f) $ 1
-- Sum {getSum = 2}

-- For Monoid Test - results match 
-- *SemigroupExercises>  f = Combine $ \n -> Sum (n + 1)
-- *SemigroupExercises> unCombine (mappend f mempty) $ 1
-- Sum {getSum = 2}

-- Step 6: quickCheck -- Not required
--see final main at end of file for full ideentity and asoc test

-- Q10 (Adapted from Q9)
newtype Comp a = Comp { unComp :: (a -> a)} 

-- Step 2: Semigroup instance 
instance (Semigroup a) => Semigroup (Comp a ) where
    Comp f <> Comp g = Comp (f <> g)
    --alternatives
--   (Comp f) <> (Comp g) = Comp (f . g)
--   f <> g = \x -> f x <> g x

-- Step 3:  Then Mondoid instance -- always return b
-- instance (Semigroup b, Monoid b) => Monoid (Combine a b) where
instance (Monoid a) => Monoid (Comp a) where  
  -- mempty = Comp mempty Not possible bcos no such scenario
  mempty = Comp id -- bcos mempty = identity case and for int , its 0 or 1 . Generalise to id here
  mappend = (<>)
-- Step 4 Arbitrary instance 
-- Use CoAribtrary to reduce a to same type as b, then you can apply fmap -NA as only 1 argu
-- instance Arbitrary (Comp a) where
--   arbitrary = fmap Comp arbitrary

-- -- Step 5: Define function  
-- compSemigroupAssoc :: (Eq a, Show a)
--                       => a
--                       -> Comp a
--                       -> Comp a
--                       -> Comp a
--                       -> Bool

-- compSemigroupAssoc x (Comp f) (Comp g) (Comp h) =
--   (f x <> (g x <> h x)) == ((f x <> g x) <> h x)

-- -- Step 6: Define the type for test
-- type CompTestInt    = Comp [Int]  -> Comp [Int]  -> Comp [Int]  -> Bool

f' = Comp $ \(Sum n) -> Sum (n + 1)

g' = Comp $ \(Sum n) -> Sum (n - 1)

res1' :: Sum Int
res1' = unComp (f' <> g') $ Sum 0 

res2' :: Sum Int
res2' = unComp (mappend f' mempty) $ Sum 0
-- Step 6: quickCheck -- Not required

-- *SemigroupExercises> res1' 
-- Sum {getSum = 0}
-- *SemigroupExercises> res2' 
-- Sum {getSum = 1}

-- Q11  - Similar to Q8
data Validation a b = Failure a | Success b deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
  (<>) (Success n) _ = Success n
  (<>) (Failure s) (Failure s') = Failure (s <> s')
  (<>) (Failure _) (Success n)  = Success n



instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    oneof [return $ Failure a, 
           return $ Success b]


main11 :: IO ()
main11 = do
  let failure :: String
               -> Validation String Int
      failure = Failure
      success :: Int
                -> Validation String Int
      success = Success
  print $ success 1 <> failure "blah"
  print $ failure "woot" <> failure "blah"
  print $ success 1 <> success 2
  print $ failure "woot" <> success 2

-- *SemigroupExercises> main11
-- Success 1
-- Failure "wootblah"
-- Success 1
-- Success 2

-- Ok, one module loaded.
-- *SemigroupExercises> Success 1 <> Failure "blah"
-- Success 1
-- *SemigroupExercises> Failure "woot" <> Failure "blah"
-- Failure "wootblah"
-- *SemigroupExercises> Success 1 <> Success 2
-- Success 1
-- *SemigroupExercises> Failure "woot" <> Success 2
-- Success 2

--see final main at end of file for full ideentity and asoc test
type ValidationTest = Validation String Int



--QUICKCHECK ALL (AGAIN)
main :: IO ()
main = do
   --Q1
   quickCheck (semigroupAssoc      :: Trivial -> Trivial -> Trivial -> Bool)
   quickCheck (monoidLeftIdentity  :: Trivial -> Bool)
   quickCheck (monoidRightIdentity :: Trivial -> Bool)
   --Q1 Alternative predefined test see Q1 above
   quickCheck (semigroupAssoc :: TrivAssoc)
   --Q2
   quickCheck (semigroupAssoc      :: Identity [Int] -> Identity [Int] -> Identity [Int] -> Bool)
   quickCheck (semigroupAssoc      :: Identity [String] -> Identity [String] -> Identity [String] -> Bool)
   quickCheck (monoidLeftIdentity  :: Identity [Int] -> Bool)
   quickCheck (monoidRightIdentity :: Identity [Int] -> Bool)
   quickCheck (monoidLeftIdentity  :: Identity [String] -> Bool)
   quickCheck (monoidRightIdentity :: Identity [String] -> Bool)
   --Q2 using defined type  
   quickCheck (semigroupAssoc      :: IdentityTestInt' ->  IdentityTestInt' -> IdentityTestInt' -> Bool)
   quickCheck (semigroupAssoc      :: IdentityTestString' -> IdentityTestString' -> IdentityTestString' -> Bool )   
   quickCheck (monoidLeftIdentity  :: IdentityTestInt' -> Bool)
   quickCheck (monoidRightIdentity :: IdentityTestInt' -> Bool)
   quickCheck (monoidLeftIdentity  :: IdentityTestString' -> Bool)
   quickCheck (monoidRightIdentity :: IdentityTestString' -> Bool)
   --Q2 another alternative See above Q2 at start of file
   quickCheck (semigroupAssoc     :: IdentityTestInt)
   quickCheck (semigroupAssoc     :: IdentityTestString)
  --Q3 using defined type
   quickCheck (semigroupAssoc      :: TwoTestSI -> TwoTestSI -> TwoTestSI -> Bool)
   quickCheck (semigroupAssoc      :: TwoTestIS -> TwoTestIS -> TwoTestIS -> Bool)
   quickCheck (monoidLeftIdentity  :: TwoTestSI -> Bool)
   quickCheck (monoidRightIdentity :: TwoTestSI -> Bool)
   quickCheck (monoidLeftIdentity  :: TwoTestIS  -> Bool)
   quickCheck (monoidRightIdentity :: TwoTestIS  -> Bool)
  --Q3 another alternative See above Q3 at start of file copied here
   quickCheck (semigroupAssoc     :: TwoTest)
  --Q4
   quickCheck (semigroupAssoc      :: ThreeTestSIF'-> ThreeTestSIF' -> ThreeTestSIF' -> Bool)
   quickCheck (semigroupAssoc      :: ThreeTestISF'-> ThreeTestISF' -> ThreeTestISF' -> Bool)
   quickCheck (semigroupAssoc      :: ThreeTestFSI'-> ThreeTestFSI' -> ThreeTestFSI' -> Bool)
   quickCheck (monoidLeftIdentity  :: ThreeTestSIF' -> Bool)
   quickCheck (monoidRightIdentity :: ThreeTestSIF' -> Bool)
   quickCheck (monoidLeftIdentity  :: ThreeTestISF' -> Bool)
   quickCheck (monoidRightIdentity :: ThreeTestISF' -> Bool)
   quickCheck (monoidLeftIdentity  :: ThreeTestFSI' -> Bool)
   quickCheck (monoidRightIdentity :: ThreeTestFSI' -> Bool)
  --Q5
   quickCheck (semigroupAssoc       :: FourTestSFIE'-> FourTestSFIE' -> FourTestSFIE' -> Bool)
   quickCheck (semigroupAssoc      :: FourTestEISF'-> FourTestEISF' -> FourTestEISF' -> Bool)
   quickCheck (semigroupAssoc      :: FourTestFSEI'-> FourTestFSEI' -> FourTestFSEI' -> Bool)
  -- no monoid for Either
  --  quickCheck (monoidLeftIdentity  :: FourTestSFIE' -> Bool)
  --  quickCheck (monoidRightIdentity :: FourTestSFIE' -> Bool)
  --  quickCheck (monoidLeftIdentity  :: FourTestEISF' -> Bool)
  --  quickCheck (monoidRightIdentity :: FourTestEISF' -> Bool)
  --  quickCheck (monoidLeftIdentity  :: FourTestFSEI' -> Bool)
  --  quickCheck (monoidRightIdentity :: FourTestFSEI' -> Bool)
   quickCheck (semigroupAssoc       :: FourTestSFIC'-> FourTestSFIC' -> FourTestSFIC' -> Bool)
   quickCheck (semigroupAssoc      :: FourTestCISF'-> FourTestCISF' -> FourTestCISF' -> Bool)
   quickCheck (semigroupAssoc      :: FourTestFSCI'-> FourTestFSCI' -> FourTestFSCI' -> Bool)
  -- OK without Either 
   quickCheck (monoidLeftIdentity  :: FourTestSFIC' -> Bool)
   quickCheck (monoidRightIdentity :: FourTestSFIC' -> Bool)
   quickCheck (monoidLeftIdentity  :: FourTestCISF' -> Bool)
   quickCheck (monoidRightIdentity :: FourTestCISF' -> Bool)
   quickCheck (monoidLeftIdentity  :: FourTestFSCI' -> Bool)
   quickCheck (monoidRightIdentity :: FourTestFSCI' -> Bool)
   --Q6
   quickCheck (semigroupAssoc :: BoolConj -> BoolConj -> BoolConj -> Bool)
   quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
   quickCheck (monoidRightIdentity :: BoolConj -> Bool)
   --Q7
   quickCheck (semigroupAssoc :: BoolDisj -> BoolDisj -> BoolDisj -> Bool)
  --  FAIL!! Lesson:  Left and Right idenity will fail on Disjunctive
   quickCheck (monoidLeftIdentity :: BoolDisj-> Bool)
   quickCheck (monoidRightIdentity :: BoolDisj -> Bool)


-- After a certain limit, u need to start another set. mainRemainder
mainRemainder :: IO ()
mainRemainder = do
  --Q8
  quickCheck (semigroupAssoc :: OrTestSI' -> OrTestSI' -> OrTestSI' -> Bool)
  quickCheck (semigroupAssoc :: OrTestCI' -> OrTestCI' -> OrTestCI' -> Bool)
  --Q9
  quickCheck (combineSemigroupAssoc :: String -> CombineTest -> CombineTest -> CombineTest -> Bool)
  quickCheck (semigroupAssoc :: ValidationTest -> ValidationTest -> ValidationTest -> Bool)

--Main
--   *SemigroupExercises> main
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
-- *** Failed! Falsified (after 1 test):  
-- BoolDisj False
-- *** Failed! Falsified (after 5 tests):  
-- BoolDisj False


--mainRemainder
-- *SemigroupExercises> mainRemainder 
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.