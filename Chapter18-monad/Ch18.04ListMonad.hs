module TwiceWhenEven where
-- see pg 1136, 1139 for specific types

-- (>>=) :: Monad m
--         => m  a -> (a -> m  b)  ->  m  b
-- (>>=) ::  [ ] a -> (a -> [ ] b) -> [ ] b

-- -- More commonly:

-- (>>=) ::     [a] -> (a -> [b]) -> [b]

-- -- Same thing as pure:
-- return :: Monad m => a ->m  a
-- return :: a -> [ ] a
-- return :: a -> [a]

twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
    x <- xs
    if even x
        then [x*x, x*x]
        else[x*x]

-- x <- xs line binds individual values out of the list input,like a list comprehension, giving us an a.  The if-then-else expression is our a -> m b. It takes the individual a values that have been bound out of our m a and can generate more values, thereby increasing the size of the list.

twiceWhenEven' :: [Integer] -> [Integer]
twiceWhenEven' xs = do
    x <- xs
    if even x
        then [x*x, x*x]
        else[]

-- >>> twiceWhenEven [1..3]
-- [1,4,4,9]


-- >>> twiceWhenEven' [1..3]
-- [4,4]
