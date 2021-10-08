module FixedMonad where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


data CountMe a =
    CountMe Integer a
    deriving (Eq,Show)

instance Functor CountMe where
    fmap f (CountMe i a) =
        -- CountMe (i+1)  (f a) -- original. Need to fix Functor and Applicative (see a)
            CountMe (i)  (f a)  -- P1168

instance Applicative CountMe where
    -- pure        = CountMe 0  -- Origina. Will not pass Monad of  CountMe _ a >>= f = f a  
    -- pure        = CountMe 1   -- p1169 Pass the Functor law but will cause Applicative [C] fail
    pure        = CountMe 0  -- to restore to valid Applicative p1170 bcos of the  (n+n')
    CountMe n f <*> CountMe n' a =
        -- CountMe (n + n') (f a) -- (original error - p1164)
        -- CountMe (n + 1) (f a) -- (lagi worse failure - p1167)
        CountMe (n + n') (f a)  -- p1168 [C]

instance Monad CountMe where
    return      = pure
    -- CountMe n a >>= f =          -- Original P1164. 
    --     let CountMe _ b = f a
    --     in CountMe ( n + 1) b
    -- CountMe _ a >>= f = f a  -- P1168 Need to change bcos need to tackle Monad error . fail identity
    CountMe n a >>= f =         -- p1170 
        let CountMe n' b = f a
        in CountMe (n + n') b

instance Arbitrary a
        => Arbitrary (CountMe a) where
    arbitrary = 
        CountMe <$> arbitrary <*> arbitrary


instance Eq a => EqProp (CountMe a) where
    (=-=) = eq

main = do
    let trigger :: CountMe (Int,String,Int)
        trigger = undefined
    quickBatch $ functor trigger
    quickBatch $ applicative trigger
    quickBatch $ monad trigger

-- *FixedMonad> main

-- functor:
--   identity: +++ OK, passed 500 tests.
--   compose:  +++ OK, passed 500 tests.

-- applicative:
--   identity:     +++ OK, passed 500 tests.
--   composition:  +++ OK, passed 500 tests.
--   homomorphism: +++ OK, passed 500 tests.
--   interchange:  +++ OK, passed 500 tests.
--   functor:      +++ OK, passed 500 tests.

-- monad laws:
--   left  identity: +++ OK, passed 500 tests.
--   right identity: +++ OK, passed 500 tests.
--   associativity:  +++ OK, passed 500 tests.
--   pure:           +++ OK, passed 500 tests.
--   ap:             +++ OK, passed 500 tests.