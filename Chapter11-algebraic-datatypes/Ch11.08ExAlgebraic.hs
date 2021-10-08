--Carinality--
--Q1 - 1
data PugType = PugData

--Q2 - 3
data Airline = PapuAir
              | CatapultsR'Us
              | TakeYourChancesUnited

--Q3 Cardinality of Int 16 
-- >>> 2* (2^15)
-- 65536

-- >>> import Data.Int
-- >>>  let x = minBound :: Int16
-- >>>  let y = maxBound :: Int16
-- >>>  (abs (fromIntegral x)) + (fromIntegral y) + 1
-- 65536
--
-- = 32768 + 32767 + 1
--  = 65536

--Q4 Cardinality of Int and Integer

-- >>> import Data.Int
-- >>>  let xInt = minBound :: Int
-- >>>  let yInt = maxBound :: Int
-- >>>  (abs (fromIntegral xInt)) + (fromIntegral yInt) + 1
-- 18446744073709551616

-- >>> import Data.Int
-- >>>  let xIntger = minBound :: Intger
-- >>>  let yIntger = maxBound :: Intger
-- >>>  (abs (xInteger)) + (yInteger) + 1
-- <interactive>:145:29-34: error:
--     Not in scope: type constructor or class `Intger'
--     Perhaps you meant `Integer' (imported from Prelude)
-- <BLANKLINE>

-- >>> minBound :: Integer
-- >>> maxBound :: Integer
-- <interactive>:308:2-9: error:
--     * No instance for (Bounded Integer)
--         arising from a use of `minBound'

-- <interactive>:309:2-9: error:
--     * No instance for (Bounded Integer)
--         arising from a use of `maxBound'

--i.e. Integer is not bounded, it is essentially infinite (limited by the available memory)

-- Q5 8 corresponds to a number of bits. A bit can hold 2 values, and when you
--    string 8 of them together, that allows 2^8, i.e. 256, different possible
--    combinations (values).

--For Example--

data Example = MakeExample deriving Show

--Q1
-- >>> :t MakeExample
-- MakeExample :: Example
--
-- >>> :t Example
-- <interactive>:1:1-7: error: Data constructor not in scope: Example
--It errors out with "Not in scope: data constructor ‘Example’" because
--    Example is a type constructor and not a data constructor

--Q2
-- >>> :i Example
-- data Example = MakeExample-- Defined at C:\Users\regina\Desktop\SMU\HaskellBook\Ch11.8ExercisesAlgebraic.hs:58:1
-- instance [safe] Show Example-- Defined at C:\Users\regina\Desktop\SMU\HaskellBook\Ch11.8ExercisesAlgebraic.hs:58:37

-- Q: What if you try :info on Example in GHCi?
-- A: It works!
-- Q: Can you determine what typeclass instances are defined for the Example
--    type using :info in GHCi?
-- A: Yep, it shows the Show typeclass instance being defined:
--    > :i Example
--    data Example = MakeExample      -- Defined at <interactive>:49:1
--    instance Show Example -- Defined at <interactive>:49:37

--Q3
data Example1 = MakeExample1 Int deriving Show
-- >>> :t MakeExample1
-- MakeExample1 :: Int -> Example1
--
-- However this only works because GHCI lets you reassign things. If you
-- put both data declarations into the same file, you get an error:
--
--     Multiple declarations of ‘MakeExample’
--     Declared at: /tmp/foo.hs:1:16
--                  /tmp/foo.hs:3:12
-- Failed, modules loaded: none.



