import Control.Applicative
-- See also Ch17.04
f x =  
    lookup x [ (3, "hello")
             , (4, "julie")
             , (5, "kbai")]

g y = 
    lookup y [ (7, "sup?")
             , (8, "chris")
             , (9, "aloha")]

h z = 
    lookup z [(2, 3), (5, 6), (7, 8)]

m x =     
    lookup x [(4, 10), (8, 13), (1, 9001)]

-- paired output is returned in a Maybe context

-- >>> f 3
-- Just "hello"
-- >>> g 8
-- Just "chris"
-- >>>  (++) <$> f 3 <*> g 7
-- Just "hellosup?"
-- >>> (+) <$> h 5 <*> m 1
-- Just 9007
-- >>>  (+) <$> h 5 <*> m 6  -- m 6 gets nothing
-- Nothing
--


-- Using liftA2
-- >>>  liftA2 (++) (g 9) (f 4)
-- Just "alohajulie"
-- >>> liftA2 (^) (h 5) (m 4)
-- Just 60466176
-- >>> liftA2 (*) (h 5) (m 4)
-- Just 60
-- >>>  liftA2 (*) (h 1) (m 1)
-- Nothing <*> Just 9001
-- Nothing
--


-- IO 
-- (++) <$> getLine <*> getLine
-- (,)  <$> getLine <*> getLine

