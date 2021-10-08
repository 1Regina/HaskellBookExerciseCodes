map id = id
-- guarantees
fmap f . fmap g = fmap (f . g)

-- >>> fmap ((+1) . (+2)) [1..5]
-- [4,5,6,7,8]
-- Prelude> fmap (+1) . fmap (+2) $ [1..5]
-- [4,5,6,7,8]

mcomp':: Monad m =>
        (b -> m c)
      ->(a -> m b)
      -> a -> m c

-- (>>=) :: Monad m => m a -> (a -> m b) -> m b
-- mcompf g a=f (g a)  This will fail. Bcos error msg: f is expecting a b for its first argument, but g is passing an m b to f. Solution is a join!
mcomp' f g a = join (f <$> (g a)) -- is equivalent to g a >>= f


mcomp'':: Monad m =>
        (b -> m c)
      ->(a -> m b)
      -> a -> m c

mcomp''f g a = g a >>= f

-- Recall composition (.)

(.) :: (b -> c) -> (a -> b) -> a -> c
    
(>>=) :: Monad m => m a -> (a -> m b) -> m b

import Control.Monad

-- the order is flipped to match >>=

(>=>) :: Monad m
       => (a -> m b) -> (b -> m c) -> a -> m c

flip (.) ::
          (a ->  b)  -> (b ->   c) -> a -> c