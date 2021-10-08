module ReaderPractice where
    
import Control.Applicative()
import Data.Maybe(fromMaybe)


x :: [Integer]
x =[1, 2, 3]
y :: [Integer]
y =[4, 5, 6]
z :: [Integer]
z =[7, 8, 9]

-- lookup :: Eq a => a -> [(a, b)] -> Maybe b


-- zip x and y using 3 as the lookup key
xs :: Maybe Integer
xs = lookup 3 $ zip x y
-- >>> zip x y
-- [(1,4),(2,5),(3,6)]

-- >>> xs
-- Just 6

-- zip y and z using 6 as the lookup key
ys :: Maybe Integer
ys = lookup 6 $ zip y z
-- >>> zip y z
-- [(4,7),(5,8),(6,9)]

-- >>> ys
-- Just 9

zs :: Maybe Integer
zs = lookup 4 $ zip x y

-- >>> lookup 4 $ zip x y
-- Nothing

-- >>> lookup 4 $ zip y z
-- Just 7

-- now zip x and z using a variable lookup key
z' :: Integer -> Maybe Integer
z' n = lookup n $ zip x z
-- >>> zip x z
-- [(1,7),(2,8),(3,9)]

-- >>> z' 3
-- Just 9

-- make a Maybe (,) of values using Applicative.
-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b
-- (<$>) :: Functor f     =>   (a -> b) -> f a -> f b

-- liftA  :: Applicative f => 
--           (a -> b)
--        ->  f a
--        ->  f b
-- liftA2 :: Applicative f =>
--            (a -> b -> c)
--         ->  f a
--         ->  f b
--         ->  f c 
-- liftA3 :: Applicative f =>
--             (a -> b -> c -> d)
--         ->  f a 
--         ->  f b 
--         ->  f c 
--         ->  f d

-- Have x1 make a tuple of xs and ys
x1 :: Maybe (Integer, Integer)
x1 = (,) <$> xs <*> ys

-- >>> x1
-- Just (6,9)

-- Have x2 make a tuple of ys and zs
x2 :: Maybe (Integer, Integer)
x2 = (,) <$> ys <*> zs

-- >>> x2
-- Nothing

-- Have x3 take one input and makes a tuple of the results of two applications of z
x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 = (,) <$> z'  <*> z' 
-- >>> x3 3
-- (Just 9,Just 9)

-- uncurry to allow us to add the two values that are inside a tuple
-- uncurry :: (a -> b -> c) -> (a, b) -> c

-- summed is uncurry with addition as the first argument:
summed  :: Num c =>(c, c) -> c
summed = uncurry (+)

-- lifts a Boolean function over two partially applied functions
-- use &&, >3, <8
bolt :: Integer -> Bool 
bolt = (&&) <$> (>3) <*> (<8)

-- >>> bolt 5
-- True

-- give it a default value and a Maybe value. If the Maybe value is a Just a, it will return the a value. If the value is Nothing, it returns the default value instead:
-- Rem x1 bcos u want Maybe (,)

-- fromMaybe :: a -> Maybe a -> a

-- *ReaderPractice> :t sequenceA
-- sequenceA :: (Traversable t, Applicative f) => t (f a) -> f (t a)
-- We have a Reader for the Applicative(functions) and a traversable for the list.
sequA :: Integral a => a -> [Bool]
sequA = sequenceA [(>3), (<8), even]
-- >>> sequA 7
-- [True,True,False]



s' :: Maybe Integer
s' = summed <$> ((,) <$> xs <*> ys)
-- >>> s'
-- Just 15

main :: IO ()
main = do
  print $ sequenceA [Just 3, Just 2, Just 1]
--   Just [3,2,1]
  print $ sequenceA [x, y]
--   [[1,4],[1,5],[1,6],[2,4],[2,5],[2,6],[3,4],[3,5],[3,6]]
  print $ sequenceA [xs, ys]
-- Just [6,9]
  

  print $ summed <$> ((,) <$> xs <*> ys)
--   Just 15
  print $ fmap summed ((,) <$> xs <*> zs)
--   Nothing
  print $ bolt 7
--   True
  print $ fmap bolt z
--   [True,False,False]

-- *ReaderPractice> main
-- Just [3,2,1]
-- [[1,4],[1,5],[1,6],[2,4],[2,5],[2,6],[3,4],[3,5],[3,6]]
-- Just [6,9]
-- Just 15
-- Nothing
-- True
-- [True,False,False]

  print $ sequenceA [(>3), (<8), even] 7  -- equal to sequA 7
  putStrLn $ "Ex 1:"
  print $ foldr (&&) True $ sequA 7
  putStrLn $ "Ex 1 alternative:"
  print $ and $ sequA 7
  putStrLn $ "Ex 2:"
  print $ sequA $ fromMaybe 0 s'  -- need to import Data.Maybe(fromMaybe)
  putStrLn $ "Ex 3:"
  print $ bolt $ fromMaybe 0 ys