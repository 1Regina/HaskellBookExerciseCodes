module MonadInstances where

import Control.Applicative

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Test.Hspec
import Data.Monoid ((<>))

-- Q1, Nope

data Nope a = NopeDotJpg deriving (Eq, Show)
instance Eq a => EqProp (Nope a) where (=-=) = eq
instance Arbitrary a => Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance Functor Nope where
  fmap _ _ = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  (<*>) _ _ = NopeDotJpg

instance Monad Nope where
  _ >>= _ = NopeDotJpg

checkNope = do
  quickBatch $ functor (undefined :: Nope (Int, Int, Int))
  quickBatch $ applicative (undefined :: Nope (Int, Int, Int))
  quickBatch $ monad (undefined :: Nope (Int, Int, Int))


-- Q2, Either

data Meither b a = 
      MLeft a 
    | MRight b 
    deriving (Eq, Ord, Show)

instance (Eq a, Eq b) => EqProp (Meither b a) where (=-=) = eq
instance (Arbitrary a, Arbitrary b) => Arbitrary (Meither b a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    oneof [ return $ MLeft a, 
            return $ MRight b ]

instance Functor (Meither b)  where
  fmap f (MLeft a)  = MLeft  (f a)
  fmap _ (MRight b) = MRight b -- Rem first argument in two cases

instance Applicative (Meither b) where
  pure  = MLeft
  (<*>) (MLeft f) (MLeft a)    = MLeft (f a)
  (<*>) (MLeft f) (MRight b)   = MRight b
  (<*>) (MRight b) (MLeft a)   = MRight b --- in the face where Right is avail, go for Right
  (<*>) (MRight b) (MRight b') = MRight b --- in the face where Right is avail, go for Right n as only 1, go for the first

instance Monad (Meither b) where
  return = pure
  (MLeft  a) >>= f = f a
  (MRight b) >>= _ = MRight b

checkMeither = do
  quickBatch $ functor (undefined :: Meither Int (Int, Int, Int))
  quickBatch $ applicative (undefined :: Meither Int (Int, Int, Int))
  quickBatch $ monad (undefined :: Meither Int (Int, Int, Int))


-- Q3

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance (Eq a) => EqProp (Identity a) where (=-=) = eq
instance (Arbitrary a) => Arbitrary (Identity a) where 
  -- arbitrary = do
  --   a <- arbitrary
  --   return $ Identity a
  -- another way to write
  arbitrary = do
    Identity <$> arbitrary

instance Functor Identity where
   fmap f (Identity a) = Identity (f a)   

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity a) = Identity (f a)

instance Monad Identity where
  return = pure
  (Identity a) >>= f = f a

checkId = do
  quickBatch $ functor (undefined :: Identity (Int, Int, Int))
  quickBatch $ applicative (undefined :: Identity (Int, Int, Int))
  quickBatch $ monad (undefined :: Identity (Int, Int, Int))


-- Q4 -- challenging 
data List a = Nil | Cons a (List a) deriving (Eq, Show)
instance (Eq a) => EqProp (List a) where (=-=) = eq 

listGen :: Arbitrary a => Gen (List a)
listGen = do
  a <- arbitrary
  frequency [(1, return Nil), (10, (Cons a) <$> listGen)] -- type with type 
instance Arbitrary a => Arbitrary (List a) where
  arbitrary = listGen

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a as) = Cons (f a) (fmap f as)

-- more for Applicative

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil


instance Applicative List where
  pure a = Cons a Nil
  (<*>) _ Nil = Nil
  (<*>) Nil _ = Nil
  (<*>) fs xs = concat' $ fmap (\f -> fmap f xs) fs

-- need to provide >>= :: List a -> (a -> List b) -> List b
instance Monad List where
  Nil >>= _ = Nil
  (Cons a as) >>= f = append (f a) (as >>= f)
  -- --alternatve
  -- (>>=) xs f = concat' $ fmap f xs

checkList = do
  quickBatch $ functor (undefined :: List (Int, Int, Int))
  quickBatch $ applicative (undefined :: List (Int, Int, Int))
  quickBatch $ monad (undefined :: List (Int, Int, Int))


-- Write the following functions using the methods provided by Monad and Functor. Using stuff like identity and compositionis fine, but it has to type check with the types provided:

-- Q1
j :: Monad m => m (m a) -> m a
j x = x >>= id -- strip the j (see  textbook expected output)

-- Q2
l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap
-- (fmap :: Functor f => (a -> b) -> f a -> f b)

-- Q3
l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
-- fmap f ma :: m (a -> b), and then i've got an m b, so i just wanna slam those together...
l2 f ma mb = (fmap f ma) <*> mb
-- aka liftM2 (from Control.Monad) -- import Control.Applicative (liftA2)

-- Q4
a :: Monad m => m a -> m (a -> b) -> m b
-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b
-- replace f with m
-- a = flip ap   -- import Control.Monad (ap)
a x f = f <*> x

-- Q5
meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh []     _ = return []
meh (x:xs) f = (fmap (:) (f x)) <*> (meh xs f)
-- so if i apply f to the head, a, i guess (f a) of type m b
-- if i recurse on the tail, (meh as f) has type m [b]
-- so then, how do i combine an m b and an m [b]?
-- well, i want to lift (:), giving me an m b -> m [b] -> m [b]

-- Q6
flipType :: Monad m => [m a] -> m [a]
flipType xs = meh xs id

---

test :: IO ()
test = hspec $ do

  describe "j" $ do
    it "works as expected" $ do
      j [[1, 2], [], [3]]                `shouldBe` [1, 2, 3]
      j (Just (Just 1))                  `shouldBe` Just 1
      j (Just (Nothing :: Maybe Char))   `shouldBe` Nothing
      j (Nothing :: Maybe (Maybe Float)) `shouldBe` Nothing

  describe "l1" $ do
    it "works as expected" $ do
      l1 length (Just [1..10])         `shouldBe` Just 10
      l1 length (Nothing :: Maybe [a]) `shouldBe` Nothing

  describe "l2" $ do
    it "works as expected" $ do
      let f c r = take r (repeat c)
      l2 f (Just 'x') (Just 5)              `shouldBe` Just "xxxxx"
      l2 f (Just 'x') Nothing               `shouldBe` Nothing
      l2 f (Nothing :: Maybe Char) (Just 5) `shouldBe` Nothing

  describe "a" $ do
    it "works as expected" $ do
      let f x = take x (repeat '.')
      a (Just 5) (Just f)                          `shouldBe` Just "....."
      a (Just 5) (Nothing :: Maybe (Int -> Float)) `shouldBe` Nothing
      a Nothing  (Just f)                          `shouldBe` Nothing

  describe "meh" $ do
    it "works as expected" $ do
      let f x = if even x then Just (take x (repeat '.')) else Nothing
      meh [2,4]   f `shouldBe` Just ["..", "...."]
      meh [2,4,1] f `shouldBe` Nothing

  describe "flipType" $ do
    it "works as expected" $ do
      flipType [Just 5, Just 10] `shouldBe` Just [5, 10]
      flipType [Just 5, Nothing] `shouldBe` Nothing

-- [1 of 1] Compiling MonadInstances   ( Ch18.07MonadInstances.hs, interpreted )
-- Ok, one module loaded.
-- *MonadInstances> test

-- j
--   works as expected
-- l1
--   works as expected
-- l2
--   works as expected
-- a
--   works as expected
-- meh
--   works as expected
-- flipType
--   works as expected

-- Finished in 0.0034 seconds
-- 6 examples, 0 failures
-- *MonadInstances> 