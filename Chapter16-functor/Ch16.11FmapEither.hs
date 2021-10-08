-- Step 1
incIfRight :: Num a 
           => Either e a
           -> Either e a

incIfRight (Right n) = Right $ n+1
incIfRight (Left e)  = Left e 

showIfRight :: Show a
           => Either e a
           -> Either e String

showIfRight (Right s) = Right $ show s
showIfRight (Left e)  = Left e

incEither :: Num a
          => Either e a
          -> Either e a
incEither m = fmap (+1) m 

showEither :: Show a
           => Either e a
           -> Either e String
showEither s = fmap show s

-- Step 2
-- Some shrinking to remove the extra argument for incEither and showEither 
incEither' :: Num a 
           => Either e a
           -> Either e a
incEither'  = fmap (+1)  

showEither' :: Show a
           => Either e a
           -> Either e String
showEither'  = fmap show 

-- Step 3
-- Generalize to beyond either
liftedInc :: (Functor f,Num b)
          => f b -> f b
liftedInc = fmap (+1) 

liftedShow :: (Functor f,Show a)
           => f a -> f String
liftedShow = fmap show

-- Exercise for my own Either twin
data Either a b = Left a | Right b 

data Sum a b = 
      First a
    | Second b 
    deriving(Eq,Show)

sumIt :: Num a
    => Sum a b
    -> Sum a b 

sumIt  = fmap (+)

-- Q1
instance Functor (Sum a) where 
    fmap (+) First a = First $ (+) a 
    fmap (+) Second b = Second b

-- Q2
-- Not possible to implement Functor since its kind is * (and not * -> *) as Left b (ie Second b) is a constant ie cannot reduced further.