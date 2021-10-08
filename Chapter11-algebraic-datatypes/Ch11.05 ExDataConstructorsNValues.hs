--Q1 Type Constructor

data Doggies a = Husky a | Mastiff a deriving(Eq,Show)

--Q2
-- >>> :k Doggies
-- Doggies :: * -> *

--Q3 String
-- >>>:k Doggies String
-- Doggies String :: *
-- Q4 Number
-- >>> :t Husky 10
-- Husky 10 :: Num a => Doggies a
--Q5 Integer
-- >>> :t Husky (10 :: Integer)
-- Husky (10 :: Integer) :: Doggies Integer
-- Q6 String
-- >>> :t Mastiff "Scooby Doo"
-- Mastiff "Scooby Doo" :: Doggies [Char]

-- Q7: Both a type and a data constructor
data DogueDeBordeaux doge = DogueDeBordeaux doge --(pg 594 + 595)

-- Q8 :t DogueDeBordeaux is a -> DogueDeBordeaux a
-- >>> :t DogueDeBordeaux
-- DogueDeBordeaux :: doge -> DogueDeBordeaux doge
--
--Q9 fDogueDeBordeaux "doggie!" is a string type
-- >>> :t DogueDeBordeaux "doggie!"
-- DogueDeBordeaux "doggie!" :: DogueDeBordeaux [Char]
--


