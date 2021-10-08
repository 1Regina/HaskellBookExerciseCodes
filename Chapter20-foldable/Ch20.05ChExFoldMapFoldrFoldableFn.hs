module Library_Functions where
import Data.Monoid 

-- class Foldable (t:: * -> *) where
--     fold    :: Monoid m => t m -> m
--     foldMap :: Monoid m => (a -> m) -> t a -> m
    
-- Q1
sum :: (Foldable t, Num a) => t a -> a
sum = foldr (+) 0

sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum.foldMap Sum

-- Q2
product :: (Foldable t,Num a) => t a -> a
product = foldr (*) 1

product' :: (Foldable t,Num a) => t a -> a
product' = getProduct . foldMap Product

-- Q3
elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem a = getAny. foldMap (\x -> Any (x == a)) 

-- Q4
minimum :: (Foldable t, Ord a) => t a -> Maybe a
minimum = foldr fmin Nothing 
    where fmin x Nothing    = Just x 
          fmin x (Just y)   = Just (min x y)

-- Q5
maximum :: (Foldable t, Ord a) => t a -> Maybe a
maximum = foldr fmax Nothing
    where fmax x Nothing    = Just x
          fmax x (Just y)   = Just (max x y)

-- Q6
null :: (Foldable t) => t a -> Bool
null = foldr (\_ _ -> False) True

null' :: (Foldable t) => t a -> Bool
null' = getAny . foldMap (\_ -> Any True)

-- Q7
length :: (Foldable t) => t a -> Int
-- foldr (\_ y -> y+1) 0
length xs = foldr (\_ acc -> acc + 1) 0 xs

length' :: (Foldable t) => t a -> Int
length' = getSum.foldMap (\_ -> Sum 1)  -- [Sum as in the Monoid Sum or Product. Each element is assigned 1 so getSum all the 1 for each element]
-- λ> foldMap Sum [1..4]
-- Sum {getSum = 10}


--Q8
toList :: (Foldable t) => t a -> [a]
toList = foldr (\x xs -> x:xs) []

toList' :: (Foldable t) => t a -> [a]
-- toList' = foldMap (\x -> [x])
toList' = foldMap (: [])

-- Q9
fold :: (Foldable t, Monoid m) => t m -> m
fold = foldMap (id)

-- Q10
-- instance Foldable Identity where
--     foldr   f z (Identity x) = f x z
--     foldl   f z (Identity x) = f z x
--     foldMap f (Identity x)   = f x

-- class Functor f => Foldable f where
--     fold    :: Monoid m =>             f m -> m
--     foldMap :: Monoid m => (a -> m) -> f a -> m
--     foldMap g a = fold $ fmap g a  

foldMap :: (Foldable t,Monoid m) => (a -> m) -> t a -> m
-- foldMap g = mconcat.fmap g
foldMap f = foldr (\x acc -> (f x) <> acc) mempty