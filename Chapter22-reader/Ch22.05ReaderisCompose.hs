newtype Reader r a =
    Reader { runReader :: r -> a }

-- r is the type we’re reading in, and a is the result type of our function
-- Reader newtype has a handy runReader accessor to getthe function out of Reader

instance Functor (Reader r) where
    fmap :: (a -> b)
         -> Reader r a
         -> Reader r b
         
fmap f (Reader ra) =
    Reader $ \r -> f (ra r)

-- same as (.)
compose :: (b -> c) -> (a -> b) -> (a -> c)
compose f g = \x -> f (g x)

-- Compare
\r -> f (ra r)  -- same as f  . ra
\x -> f (g  x)  -- same  as f . g

instance Functor (Reader r)where
    fmap :: (a -> b)
         -> Reader r a
         -> Reader r b

fmap f (Reader ra) =
        Reader $ (f .ra)

-- Exercise
-- newtype Reader r a =
--     Reader { runReader :: r -> a }

ask :: Reader a a 
-- a is the type we are reading in and returns a type
instance Functor (Reader a) where
    fmap :: (x -> y)
         -> Reader a x
         -> Reader a y

fmap f (Reader a) = Reader (f . a)

ask = Reader id
