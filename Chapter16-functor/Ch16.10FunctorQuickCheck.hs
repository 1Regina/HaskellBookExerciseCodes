{-# LANGUAGE ViewPatterns #-}
module FunctorInstances where

-- from section 16.9, for QuickCheck helpers
import Test.QuickCheck 
import Test.QuickCheck.Function 

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

-- 2 argument scenarios tuples, or ?? Applicative?
functorComposition :: (Functor f, Eq (f c)) => Fun a b -> Fun b c -> f a -> Bool
functorComposition (Fun _ f) (Fun _ g) x
  = (fmap g (fmap f x)) == (fmap (g . f) x)


type IntToChar = Fun Int Char
type CharToBool = Fun Char Bool

-- Q1
newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

type IdentityTestInt = Identity Int


-- Q2
data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a a') = Pair (f a) (f a')

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    a1 <- arbitrary
    a2 <- arbitrary
    return $ Pair a1 a2

type PairTestInt = Pair Int 

-- Q3
data Two a b = Two a b deriving (Eq, Show)

-- Cases of tuple & or, ONLY 1 (f a) in type signature. So first argument (Two a) need to combine 
instance Functor (Two a) where
  fmap f (Two a b) = (Two a) (f b)

instance (Arbitrary a, Arbitrary b)  => Arbitrary (Two a b) where
  arbitrary = do
    a1 <- arbitrary
    a2 <- arbitrary
    return $ Two a1 a2

type TwoTestCharInt = Two Char Int 

--Q4
data Three a b c = Three a b c deriving (Eq, Show)

-- need to reduce to achieve 1 f a to meet functor type signature
instance Functor (Three a b) where
  fmap f (Three a b c) = (Three a b) (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c)  => Arbitrary (Three a b c) where
  arbitrary = do
    a1 <- arbitrary
    a2 <- arbitrary
    a3 <- arbitrary
    return $ Three a1 a2 a3

type ThreeTestBCI = Three Bool Char Int

-- Q5
data Three' a b = Three' a b b deriving (Eq, Show)

-- need to reduce to achieve 1 f a to meet functor type signature
instance Functor (Three' a) where
  fmap f (Three' a b b' ) = Three' a (f b) (f b')

instance (Arbitrary a, Arbitrary b)  => Arbitrary (Three' a b) where
  arbitrary = do
    a1 <- arbitrary
    b1 <- arbitrary
    b2 <- arbitrary
    return $ Three' a1 b1 b2

type Three'TestCI = Three' Char Int

-- Q6

data Four a b c d = Four a b c d  deriving (Eq, Show)

-- need to drop d otherwise kind error
instance Functor (Four a b c ) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d)
  where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      c <- arbitrary
      d <- arbitrary
      return $ Four a b c d

type FourTest = Four Bool String Char Int

-- Q7
data Four' a b = Four' a a a b  deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c (f d)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
    arbitrary = do
      a1 <- arbitrary
      a2 <- arbitrary
      a3 <- arbitrary
      b <- arbitrary
      return $ Four' a1 a2 a3 b

type Four'Test = Four' Char Int


-- Q8
data Trivial = Trivial

-- Not possible to implement Functor since its kind is * (and not * -> *)


test :: IO ()
test = do
  quickCheck (functorIdentity :: IdentityTestInt -> Bool)
  quickCheck (functorComposition :: IntToChar -> CharToBool -> IdentityTestInt -> Bool)
  quickCheck (functorIdentity :: PairTestInt -> Bool)
  quickCheck (functorComposition :: IntToChar -> CharToBool -> PairTestInt -> Bool)
  quickCheck (functorIdentity :: TwoTestCharInt -> Bool)
  quickCheck (functorComposition :: IntToChar -> CharToBool -> TwoTestCharInt -> Bool)
  quickCheck (functorIdentity :: ThreeTestBCI -> Bool)
  quickCheck (functorComposition :: IntToChar -> CharToBool -> ThreeTestBCI -> Bool)
  quickCheck (functorIdentity :: Three'TestCI -> Bool)
  quickCheck (functorComposition :: IntToChar -> CharToBool -> Three'TestCI -> Bool)
  quickCheck (functorIdentity :: FourTest -> Bool)
  quickCheck (functorComposition :: IntToChar -> CharToBool -> FourTest -> Bool)
  quickCheck (functorIdentity :: Four'Test -> Bool)
  quickCheck (functorComposition :: IntToChar -> CharToBool -> Four'Test -> Bool)
  
-- >>> test

-- *FunctorInstances> :r
-- [1 of 1] Compiling FunctorInstances ( Ch16.10FunctorQuickCheck.hs, interpreted )
-- Ok, one module loaded.
-- *FunctorInstances> test
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