
module S10_6 where
import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate   UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase = [ DbDate (UTCTime (fromGregorian 1911 5 1)
                              (secondsToDiffTime 34123)),
                DbNumber 9001,
                DbString "Hello, world!",
                DbNumber 578,
                DbDate (UTCTime (fromGregorian 1921 5 1)
                                (secondsToDiffTime 34123))]

--theDatabase is a list. Can use foldr foldl

-- 1
-- Filters for DbDate values and returns
-- a list of the UTCTime values inside them
filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr onlyDbDate []
  where onlyDbDate (DbDate x) xs = x : xs
        onlyDbDate _ xs          = xs

-- >>> filterDbDate theDatabase
-- [1911-05-01 09:28:43 UTC,1921-05-01 09:28:43 UTC]
--

-- 2
-- Filters for DbNumber values and returns
-- a list of the Integer values inside them
filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr onlyDbNumber []
  where onlyDbNumber (DbNumber x) xs = x : xs
        onlyDbNumber _ xs          = xs

-- >>> filterDbNumber theDatabase 
-- [9001,578]
--


-- 3
-- Gets the most recent date
mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = foldr maxDbDate zeroDate
  where maxDbDate (DbDate x) y = max x y
        maxDbDate _ y          = y
        zeroDate = UTCTime (fromGregorian 0 1 1) (secondsToDiffTime 0)

-- >>> mostRecent theDatabase
-- 1921-05-01 09:28:43 UTC
--

-- 4
-- Sums all of the DbNumber values
sumDbNumber :: [DatabaseItem] -> Integer
sumDbNumber = foldr addDbNumber 0
  where addDbNumber (DbNumber x) acc = x + acc
        addDbNumber _ acc            = acc

-- >>> sumDbNumber theDatabase
-- 9579
--

-- 5
-- Gets the average of the DbNumber values
avgDbNumber :: [DatabaseItem] -> Double
avgDbNumber = (\(a, c) -> (fromInteger a) / (fromInteger c)) . (foldr addDbNumber (0, 0))
  where addDbNumber (DbNumber x) (acc, count) = (acc + x, count + 1)
        addDbNumber _            (acc, count) = (acc, count)

-- >>> avgDbNumber theDatabase
-- 4789.5
--
