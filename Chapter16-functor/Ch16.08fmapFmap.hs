module LMS where 
import Data.Functor

-- lms ~ List (Maybe (String))
n = Nothing
w = Just "woohoo"
ave = Just "Ave"
lms :: [Maybe [Char]]
lms = [ave, n, w]
replaceWithP :: b -> Char 
replaceWithP = const 'p'

-- >>> :t fmap
-- fmap :: Functor f => (a -> b) -> f a -> f b
-- >>> :t (fmap.fmap)
-- (fmap.fmap)
--   :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
-- >>> :t (fmap.fmap.fmap)
-- (fmap.fmap.fmap)
--   :: (Functor f1, Functor f2, Functor f3) =>
--      (a -> b) -> f1 (f2 (f3 a)) -> f1 (f2 (f3 b))



-- -- Ok, one module loaded.
-- lms = [Just "Ave",Nothing,Just "woohoo"]
-- >>> replaceWithP lms
-- 'p'
-- >>> fmap replaceWithP lms
-- "ppp"

-- >>> (fmap.fmap) replaceWithP lms
-- [Just 'p',Nothing,Just 'p']

-- tally number of p to letters where there are strings
-- >>> (fmap.fmap.fmap) replaceWithP lms
-- [Just "ppp",Nothing,Just "pppppp"]
--
-- >>> (fmap.fmap.fmap.fmap) replaceWithP lms
-- <interactive>:1730:37-39: error:
--     • Couldn't match type ‘Char’ with ‘f a0’
--       Expected type: [Maybe [f a0]]
--         Actual type: [Maybe [Char]]


-- Analysis 
-- >>> :t lms
-- lms :: [Maybe [Char]]

-- >>> :t replaceWithP lms
-- replaceWithP lms :: Char


-- >>> :t fmap replaceWithP lms
-- fmap replaceWithP lms :: [Char]

-- >>> :t (fmap.fmap) replaceWithP lms
-- (fmap.fmap) replaceWithP lms :: [Maybe Char]

-- >>> :t (fmap.fmap.fmap) replaceWithP lms
-- (fmap.fmap.fmap) replaceWithP lms :: [Maybe [Char]]

-- cannot do 4 fmap for [Maybe [Char]]
-- >>> :t (fmap.fmap.fmap.fmap) replaceWithP lms
-- <interactive>:1:36-38: error:
--     • Couldn't match type ‘Char’ with ‘f a0’
--       Expected type: [Maybe [f a0]]
--         Actual type: [Maybe [Char]]
--     • In the second argument of ‘fmap . fmap . fmap . fmap’, namely
--         ‘lms’
--       In the expression: (fmap . fmap . fmap . fmap) replaceWithP lms


ha = Just ["Ha", "Ha"]
lmls = [ha, Nothing, Just []]
-- >>> lmls
-- [Just ["Ha","Ha"],Nothing,Just []]

-- >>> replaceWithP lmls
-- 'p'

-- >>> fmap replaceWithP lmls 
-- "ppp"

-- >>>(fmap.fmap) replaceWithP lmls 
-- [Just 'p',Nothing,Just 'p']

-- tally number of p to letters where there are strings
-- >>> (fmap.fmap.fmap) replaceWithP lmls 
-- [Just "pp",Nothing,Just ""]

--final step to revert to original structure
-- >>> (fmap.fmap.fmap.fmap) replaceWithP lmls 
-- [Just ["pp","pp"],Nothing,Just []]

--type error input after stage where type tally with original
-- >>> (fmap.fmap.fmap.fmap.fmap) replaceWithP lmls 

