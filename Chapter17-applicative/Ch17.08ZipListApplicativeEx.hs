module ListApplicative where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Eq a => EqProp (List a) where (=-=) = eq
listGen :: Arbitrary a => Gen (List a)
listGen = do
  a <- arbitrary
  frequency [(1, return Nil), (10, (Cons a) <$> listGen)]

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = listGen

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a as) = Cons (f a) (fmap f as)

take' :: Int -> List a -> List a
take' _ Nil = Nil
take' n (Cons a as)
  | n > 0 = Cons a (take' (n-1) as)
  | otherwise = Nil

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f as = concat' $ fmap f as

instance Applicative List where
  pure a = Cons a Nil
  (<*>) _ Nil = Nil
  (<*>) Nil _ = Nil
  (<*>) lf la = flatMap (\f -> fmap f la) lf

newtype ZipList' a = ZipList' (List a) deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
                in take' 3000 l
          ys' = let (ZipList' l) = ys
                in take' 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

zip' :: List (a -> b) -> List a -> List b
zip' Nil _ = Nil
zip' _ Nil = Nil
zip' (Cons f fs) (Cons a as) = Cons (f a) (zip' fs as)

repeat' :: a -> List a
repeat' a = Cons a (repeat' a)

instance Applicative ZipList' where
  pure a = ZipList' $ repeat' a
  (<*>) _ (ZipList' Nil) = ZipList' Nil
  (<*>) (ZipList' Nil) _ = ZipList' Nil
  (<*>) (ZipList' (Cons f fs)) (ZipList' (Cons a as)) = ZipList' $ Cons (f a) (zip' fs as) 

zlistGen :: Arbitrary a => Gen (ZipList' a)
zlistGen = do
  as <- arbitrary
  return $ ZipList' as
instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = zlistGen

list :: [a] -> List a
list [] = Nil
list (a:as) = Cons a (list as)

zlist :: [a] -> ZipList' a
zlist as = ZipList' $ (list as)

main = do
  quickBatch $ applicative (undefined :: ZipList' (Int, Int, Int))