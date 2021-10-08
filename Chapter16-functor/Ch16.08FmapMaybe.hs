
module ReplaceExperiment where

replaceWithP :: b -> Char
replaceWithP = const 'p'

lms :: [Maybe [Char]]
lms = [Just "Ave", Nothing, Just "woohoo"]

replaceWithP'::[Maybe[Char]]-> Char
replaceWithP' = replaceWithP


-- >>> :t fmap
-- fmap :: Functor f => (a -> b) -> f a -> f b


-- >>> :t fmap replaceWithP
-- fmap replaceWithP :: Functor f => f b -> f Char
--

liftedReplace :: Functor f => f a -> f Char
liftedReplace = fmap replaceWithP
-- 1 fmap match with no. of elements ie replace and shrink
-- >>> liftedReplace lms
-- "ppp"
--
-- >>> :t "ppp"
-- "ppp" :: [Char]
--

-- the f  becomes [ ] for the Char
liftedReplace' :: [Maybe[Char]]->[Char]
liftedReplace' = liftedReplace
-- >>> liftedReplace' lms
-- "ppp"
--

-- twice fmap gives back orig form but all string is now p
twiceLifted :: (Functor f1, Functor f) => 
               f (f1 a) -> f (f1 Char)
twiceLifted = (fmap . fmap) replaceWithP
-- >>> twiceLifted lms
-- [Just 'p',Nothing,Just 'p']
-- >>> :t [Just 'p',Nothing,Just 'p']
-- [Just 'p',Nothing,Just 'p'] :: [Maybe Char]
--

-- >>> :t (fmap.fmap)
-- (fmap.fmap)
--   :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
-- >>> :t twiceLifted
-- twiceLifted :: (Functor f1, Functor f) => f (f1 a) -> f (f1 Char)
--

twiceLifted' :: [Maybe [Char]]
             -> [Maybe Char]
twiceLifted' = twiceLifted
-- >>> :t twiceLifted'
-- twiceLifted' :: [Maybe [Char]] -> [Maybe Char]
-- >>> twiceLifted' lms
-- [Just 'p',Nothing,Just 'p']
--

-- >>> :t (fmap.fmap.fmap)
-- (fmap.fmap.fmap)
--   :: (Functor f1, Functor f2, Functor f3) =>
--      (a -> b) -> f1 (f2 (f3 a)) -> f1 (f2 (f3 b))


-- thrice fmap gets original form structure & each char becomes the 'p'
-- >>> :t (fmap . fmap . fmap) replaceWithP
-- (fmap . fmap . fmap) replaceWithP
--   :: (Functor f1, Functor f2, Functor f3) =>
--      f1 (f2 (f3 a)) -> f1 (f2 (f3 Char))
--
thriceLifted ::(Functor f2, Functor f1, Functor f) =>
             f (f1 (f2 a))->f (f1 (f2 Char))
thriceLifted = (fmap.fmap.fmap) replaceWithP
-- >>> :t thriceLifted
-- thriceLifted
--   :: (Functor f2, Functor f1, Functor f) =>
--      f (f1 (f2 a)) -> f (f1 (f2 Char))
-- >>> thriceLifted lms
-- [Just "ppp",Nothing,Just "pppppp"]
--



thriceLifted' :: [Maybe[Char]] -> [Maybe[Char]]
thriceLifted' = thriceLifted

-- >>> :t thriceLifted'
-- thriceLifted' :: [Maybe [Char]] -> [Maybe [Char]]
--
main:: IO()
main = do 
    putStr "replaceWithP' lms:   " print (replaceWithP' lms)
    putStr "liftedReplace lms:   " print (liftedReplace lms)
    putStr "liftedReplace' lms:  " print (liftedReplace' lms)
    putStr "twiceLifted lms:     " print (twiceLifted lms)
    putStr "twiceLifted' lms:    " print (twiceLifted' lms)
    putStr "thriceLifted lms:    " print (thriceLifted lms)
    putStr "thriceLifted' lms:   " print (thriceLifted' lms)

-- >>> main
