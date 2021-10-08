-- --2 ways to have Ord
-- data DayOfWeek1 = 
--     Mon | Tue | Weds | Thu | Fri | Sat | Sun
--     deriving (Ord, Show) -- ONLY derive Ord, Show if still have Eq instances (below) from previous ow bring back by putting into the earlier file or derive an Eq instance

-- -- Eq instances
-- instance Eq DayOfWeek1 where
--     -- Indent is impt
--     (==) Mon Mon = True
--     (==) Tue Tue = True
--     (==) Weds Weds = True
--     (==) Thu Thu = True
--     (==) Fri Fri = True
--     (==) Sat Sat = True
--     (==) Sun Sun = True


data DayOfWeek2 = 
    Mon | Tue | Weds | Thu | Fri | Sat | Sun
    deriving (Eq, Show)

--Ord instances (see also Eq instances above for alternative way)
instance Ord DayOfWeek2 where
    -- Indent is impt
    compare Fri Fri = EQ -- equal
    compare Fri _   = GT -- greater than
    compare _ Fri   = LT -- less than
    compare _ _     = EQ -- equal
-- >>> compare Fri Sat
-- GT
-- >>> compare Sat Mon
-- EQ
-- >>> Mon > Fri
-- False
-- >>> Fri > Sat
-- True
-- Test of Eq instances
-- >>> Sat == Mon
-- False
--

-- check' :: a -> a -> Bool
-- check' a a' = a == a'  --fails becos lack Eq in type signature above

check'' :: Ord a => a -> a -> Bool
check'' a a' = a == a'   --compiles bcos  instances of Ord also would have taken care of Instance of Eq