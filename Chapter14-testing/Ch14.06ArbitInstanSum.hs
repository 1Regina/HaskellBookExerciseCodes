module ArbitInstanSum where

import Test.QuickCheck
import Test.QuickCheck.Gen (oneof)


data Sum a b = First a| Second b deriving(Eq,Show)
-- equal odds for each
sumGenEqual::(Arbitrary a,Arbitrary b) => Gen (Sum a b)
sumGenEqual = do 
    a <- arbitrary
    b <- arbitrary
    oneof [return $ First a, --oneof means choose either one
           return $ Second b]


sumGenCharInt:: Gen (Sum Char Int)
sumGenCharInt = sumGenEqual -- as a b are arbitrary polymorphic

-- *Main> sample sumGenCharInt
-- Second 0
-- First 'd'
-- Second (-1)
-- Second (-2)
-- First '\SI'
-- Second (-8)
-- First '6'
-- First '\ETX'
-- Second 8
-- First '\RS'
-- Second 15


sumGenFirstPls :: (Arbitrary a, Arbitrary b) => Gen (Sum a b)
sumGenFirstPls = do
  a <- arbitrary
  b <- arbitrary
  frequency [(10, return $ First a), (1, return $ Second b)]

sumGenCharIntFirst :: Gen (Sum Char Int)
sumGenCharIntFirst = sumGenFirstPls


-- *ArbitInstanSum> sample sumGenCharIntFirst
-- First '\891185'
-- First 'A'
-- First 'u'
-- First 'K'
-- First 'q'
-- First '\399720'
-- First '$'
-- First 'y'
-- First '#'
-- First '\ACK'
-- First '\RS'