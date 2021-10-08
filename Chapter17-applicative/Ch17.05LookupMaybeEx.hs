import Data.List (elemIndex)

-- Make changes to the functions with the follow so they typecheck
-- pure
-- pure :: Applicative f => a -> f a
-- -- (<$>)
-- (<$>) :: Functor f => (a -> b) -> f a -> f b -- f as the list / structure
-- -- (<*>)
-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b

-- Q1
added :: Maybe Integer
added = 
   fmap (+3) (lookup 3 $ zip [1, 2, 3] [4, 5, 6]) -- add fmap

-- >>> zip [1, 2, 3] [4, 5, 6]
-- [(1,4),(2,5),(3,6)]
-- >>> lookup 3 $ zip [1, 2, 3] [4, 5, 6]
-- Just 6
-- >>> fmap (+3) (lookup 3 $ zip [1, 2, 3] [4, 5, 6])
-- Just 9
-- >>> added
-- Just 9
--

-- Q2
y :: Maybe Integer
y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]
z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer,Integer)
tupled = (,) <$> y <*> z -- added <$> and <*> 

-- Compare from earlier section
-- >>> (,) <$> [1, 2] <*> [3, 4]
-- [(1,3),(1,4),(2,3),(2,4)]

-- >>> y
-- Just 6
-- >>> z
-- Just 5
-- >>> tupled
-- Just (6,5)
--
tupled' :: Maybe (Integer, Integer)
tupled' = ((,) <$> y) <*> z
-- >>> tupled'
-- Just (6,5)
--
tupled1 :: Maybe (Integer, Integer)
tupled1 = pure (,) <*> y <*> z
-- >>> tupled1
-- Just (6,5)
--

-- Q3

x :: Maybe Int
x = elemIndex 3 [1, 2, 3, 4, 5]

y' :: Maybe Int
y' = elemIndex 4 [1, 2, 3, 4, 5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> x <*> y' -- added <$> and <*>

-- >>> x
-- Just 2
-- >>> y'
-- Just 3
-- >>> maxed
-- Just 3
--


-- Q4
xs = [1, 2, 3]
ys = [4, 5, 6]

x'' :: Maybe Integer
x'' = lookup 3 $ zip xs ys

y'' :: Maybe Integer
y'' = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = pure sum <*> ( (,) <$> x'' <*> y'') -- refer to Q2 . After it becomes a tuple, fmap will only take the second argu to apply

-- >>> zip xs ys
-- [(1,4),(2,5),(3,6)]

-- >>> x''
-- Just 6
-- >>> y''
-- Just 5
-- >>> (,) <$> x'' <*> y''
-- Just (6,5)
-- >>> summed
-- Just 5
--

