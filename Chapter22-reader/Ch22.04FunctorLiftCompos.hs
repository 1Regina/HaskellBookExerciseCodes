-- -- Type constructor of functions
-- (->)
-- -- Fully applied
-- a -> b

-- ((->) r)
-- -- is

-- r ->

-- r, above, is the type of the argument to the function
-- 1) is part of the structure being lifted over when we lift over a function, not the value being transformed or mapped over.
-- 2) This leaves the result of the function as the value being transformed 
-- ie function composition

-- Proof fmap = (.)
instance Functor ((->) r) where
    fmap = (.)

(.) ::(b -> c) -> (a -> b) -> a -> c
(.)::   (b -> c) -> (a -> b) -> (a -> c)
fmap :: Functor f => (a -> b) -> f a -> f b 
fmap :: (b -> c) -> f b      -> f c

f is ((->) a)
see fmap
:: (b -> c)
->       (a -> b)
->       (a -> c)

:: (b -> c)
->((->) a)   b
->((->) a)   c

-- Unroll the prefix notation into infix to compare (.) with rolled out fmap
fmap' :: (b -> c) -> (a -> b) -> (a -> c) 
(.)   :: (b -> c) -> (a -> b) -> (a -> c)  

