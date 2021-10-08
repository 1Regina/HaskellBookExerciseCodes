import Data.Monoid

-- See also Ch17.04

-- (<*>) :: f   (a -> b) -> f  a  -> f  b
-- (<*>) :: [ ] (a -> b) -> [ ] a -> [ ] b

-- more syntactically typical

-- (<*>) :: [(a -> b)] -> [a] -> [b]
-- pure  :: a -> f  a
-- pure  :: a -> [ ] a


-- >>>  [(+1), (*2)] <*> [2, 4]
-- [ (+1) 2 , (+1) 4 , (*2) 2 , (*2) 4 ]
-- [3,5,4,8]
--
(<*>):: Applicative f => f (a -> b) -> f a -> f b

f ~ []

listApply :: [(a -> b)] -> [a] -> [b]
listFmap  :: (a -> b) -> [a] -> [b]
