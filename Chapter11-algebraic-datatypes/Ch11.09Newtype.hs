newtype Goats = Goats Int deriving Show
class TooMany a where tooMany :: a-> Bool
instance TooMany Int where tooMany n = n>42
instance TooMany Goats where tooMany (Goats n) = n>43

-- >>> :t tooMany
-- tooMany :: TooMany a => a -> Bool

-- >>> tooMany (Goats 45)
-- True
--
-- >>> tooMany (Goats 5)
-- False
--
-- Q1
newtype IntString = IntString (Int, String)
-- have to insert newype bcos  get the "FlexibleInstances" the book talks about
-- reading the error message, it basically says the type and data constructor not in scope
-- and that's where the `newtype` suggestion comes in i guess

instance TooMany IntString where tooMany (IntString (n, string)) = (length string) > n

--Q2
newtype IntSum = IntSum (Int, Int)
instance TooMany IntSum where tooMany (IntSum (n1 , n2)) = n1 + n2 > 42

-- Or instance TooMany (Int, Int) where tooMany x = (fst x) + (snd x) > 42

--Q3 tough one. 
-- instance  (Num a, TooMany a) => (a,a)
{-# LANGUAGE FlexibleInstances #-}
-- newtype IntSum' = IntSum' (Int, Int) deriving (Eq, Show, Num, TooMany)
-- instance (Num a, TooMany a) => TooMany (a, b) where tooMany (x, y) = tooMany $x + y

-- instance (Num a, TooMany a) => TooMany (a, a) where tooMany x = tooMany (fst x) || tooMany (snd x)
 -- >>> TooMany (4,5)
 -- <interactive>:464:2-8: error:
 --     • Data constructor not in scope: TooMany :: (Integer, Integer) -> t
 --     • Perhaps you meant variable ‘tooMany’ (line 2)
 --

  -- so what just happened?
-- I think the instance for TooMany (a, a) is set up as intended. It requires the FlexibleInstances pragma.
-- to test that instance, we need a type that is both TooMany and Num, so we created
-- Goat'. It picks up the Num instance automatically from the inner Int type, and then
-- we also added the instance TooMany Int so it could automatically get that instance too.
-- Now you can call tooMany (Goats' 3, Goats' 7)
