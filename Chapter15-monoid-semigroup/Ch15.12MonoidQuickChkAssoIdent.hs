import Control.Monad ()
import Data.Monoid ()
import Test.QuickCheck --( oneof, quickCheck, Arbitrary(arbitrary) )

-- from 15.12 Associativity
monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

---
--From earlier Section Ch15.10
-- refer pg 899

data Optional a = Nada | Only a deriving (Eq, Show)

--Ch15.12exercise
newtype First' a = First' { getFirst' :: Optional a } deriving (Eq, Show)

-- see work done on Semigroup (Optional a) Ch15.10
instance Semigroup (First' a) where
    (<>) x (First' Nada) = x
    (<>) (First' Nada) x = x
    (<>) (First' (Only a)) _ = First' (Only a)
    

instance Monoid (First' a) where
  mempty  = First' Nada
  mappend (First' Nada) x = x
  mappend (First' (Only a)) _ = First' (Only a)
  -- mappend x             _ = x  -- another way to write
  -- mappend x@(First' (Only a)) _ = x  -- another way to write

firstMappend :: Monoid a => First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend = First' String -> First' String -> First' String -> Bool
type FstId = First' String -> Bool


-- some gist on aribitrary and gen
-- :t arbitrary :: Arbitrary a => Gen a
-- :t frequency
-- refer to Ch14.06ArbitInstanSum.hs sumGenFirstPls 

-- req'd by qn
-- some gist on aribitrary and gen
-- :t arbitrary :: Arbitrary a => Gen a
-- :t frequency
-- refer to Ch14.06ArbitInstanSum.hs sumGenFirstPls for frequency
instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = do
    a <- arbitrary
    -- either oneof or freq
    -- oneof [return $ First' Nada,
    --        return $ First' (Only a)]
    frequency [(1, return $ First' Nada), (10, return $ First' (Only a))] --to change odds

qcTestFirstMonoid :: IO ()
qcTestFirstMonoid = do
  quickCheck (monoidAssoc :: First' String -> First' String -> First' String -> Bool)
  quickCheck (monoidLeftIdentity :: First' Int -> Bool)
  quickCheck (monoidRightIdentity :: First' Char -> Bool)

qcTestMonoid :: IO ()
qcTestMonoid = do
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: FstId)
  quickCheck (monoidRightIdentity :: FstId)

-- *Main> qcTestFirstMonoid 
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
-- *Main> qcTestMonoid
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.