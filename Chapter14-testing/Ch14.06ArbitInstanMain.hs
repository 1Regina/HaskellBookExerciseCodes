-- Ch14.6 Arbitrary instances
module Main where

import Test.QuickCheck 
import Test.QuickCheck.Gen(oneof)--oneof added for sum test

data Trivial=Trivial
             deriving(Eq,Show)


trivialGen :: Gen Trivial
trivialGen = return Trivial

instance Arbitrary Trivial where arbitrary = trivialGen
--The return is necessary to return Trivial in the Gen monad:

main:: IO()
main = do sample trivialGen

-- *Main> :l ArbitInstanMain.hs
-- *Main> sample' trivialGen
-- [Trivial,Trivial,Trivial,Trivial,Trivial,Trivial,Trivial,Trivial,Trivial,Trivial,Trivial]


-- Ch14.6 Identity Crisis
data Identity a =
    Identity a
    deriving(Eq,Show)

identityGen :: Arbitrary a =>
                Gen(Identity a)
identityGen = do 
    a <- arbitrary
    return (Identity a)


instance Arbitrary a => 
         Arbitrary(Identity a) where
    arbitrary = identityGen

identityGenInt:: Gen(Identity Int)
identityGenInt=identityGen
-- *Main> sample' identityGenInt
-- [Identity 0,Identity (-2),Identity (-2),Identity (-2),Identity (-4),Identity 6,Identity (-10),Identity 4,Identity (-8),Identity 6,Identity (-4)]

identityGenFloat:: Gen(Identity Float)
identityGenFloat=identityGen
-- *Main> sample' identityGenFloat
-- [Identity 0.0,Identity (-1.9464787),Identity (-1.8162493e-3),Identity 4.014686,Identity 3.7340035,Identity (-0.9886895),Identity (-10.9949665),Identity 6.683374,Identity (-3.4858048),Identity 13.709958,Identity 17.264772]

identityGenDouble:: Gen(Identity Double)
identityGenDouble=identityGen

-- Ch14.6 Arbitrary products

data Pair a b =
    Pair a b 
    deriving(Eq,Show)

pairGen :: (Arbitrary a, Arbitrary b) =>
            Gen(Pair a b)
pairGen= do
    a <- arbitrary
    b <- arbitrary
    return (Pair a b)


instance (Arbitrary a, Arbitrary b) =>
        Arbitrary (Pair a b) where
        arbitrary = pairGen

pairGenIntString :: Gen (Pair Int String)
pairGenIntString = pairGen
-- *Main> sample pairGenIntString
-- Pair 0 ""
-- Pair 1 "\ETX\1104562"
-- Pair 4 "'\DLE"
-- Pair 3 "\490313\SOH}oI"
-- Pair 8 "\904333v"
-- Pair (-1) "'y&\ENQO\528165oW"
-- Pair 9 ""
-- Pair (-14) "i\187044i>Q\FSr"
-- Pair 1 "}\51806\780638\vk>R\ETX\1083283\fN"
-- Pair 7 "\192008m*dj\300066ug\380025\&86\CANPg\1104368X"
-- Pair (-6) "i?<z\vnQ\688552\&8\DLE\SYNWE\b>\191327m"




