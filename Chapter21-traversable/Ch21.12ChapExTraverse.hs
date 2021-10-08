-- Traversable instances
-- Write a Traversable instance for the datatype provided, filling in any required superclasses. Use QuickCheckto validate yourinstances
module Instances ( Identity
                 , Constant
                 , Optional
                 , List
                 , Three
                 , Pair
                 , Big
                 , Bigger
                 , S
                 , Tree 
                 ) where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


-- traverse
--   :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)
-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b
-- (<$>) :: Functor f => (a -> b) -> f a -> f b
-- (>>=) :: Monad m => m a -> (a -> m b) -> m b
-- -- Compare fmap, flip bind and traverse:
-- -- fmap     :: (a -> b)   -> f a -> f b
-- -- (=<<)    :: (a -> m b) -> m a -> m b
-- -- traverse :: (a -> f b) -> t a -> f (t b)
-- -- mapM     :: (a -> m b) -> [a] -> m [b]
-- foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m

-- Refer to Ch21.09 also for steps and instances
-- 1. Identity
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  --fmap :: (a -> b) -> Identity a -> Identity b
    fmap     f   (Identity a)      = Identity $ f a

-- The following Applicative isn't necessary
instance Applicative Identity where
    -- (<*>) :: Applicative f => f (a -> b) -> f a -> f b
    pure                      = Identity
    Identity f <*> Identity a = Identity (f a)

instance Foldable Identity where
    -- foldMap ::(a -> m) -> t a - > m
    -- foldMap :: Monoid m => (a -> m) -> Identity a - > m
    foldMap   f     (Identity a)   = Identity $ f a

    -- foldr :: (a -> b -> b) -> b -> Identity a -> b
    foldr     f  z  (Identity a)   = Identity $ f a z

instance Traversable Identity where 
    -- traverse :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)
    -- traverse :: (a -> f b) -> t a -> f (t b)
    -- traverse :: Applicative f => (a -> f b) -> Identity a -> f (t b)
    traverse  f  (Identity a)      = Identity <$> f a -- or fmap Identity $ f a

-- see p1097 or p1102 or p1165
instance Eq a => EqProp (Identity a) where (=-=) = eq
instance Arbitrary a => Arbitrary (Identity a) where
    -- arbitrary :: Gen (Identity a)
    -- Identity :: a -> Identity a
    arbitrary = do Identity <$> arbitrary 
    -- more familiarly seen as 
    -- arbitrary = do
    --     a <- arbitrary
    --     return $ Identity a

testIdentity = do
  quickBatch $ functor (undefined :: Identity (Int, Int, Int))
  quickBatch $ traversable (undefined :: Identity (Int, Int, [Int]))

-- 2. Constant
newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  --fmap :: (a -> b) -> Constant a -> Constant a
    fmap     _   (Constant a )      =  Constant a 
    -- fmap     f   (Constant a )      =  Constant a --as above. no need to duplicate

instance Foldable (Constant a) where
    -- foldMap ::(a -> m) -> t a - > m
    -- foldMap :: Monoid m => (a -> m) -> t a -> m
    foldMap  _    _                 = mempty
    -- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
    -- foldr :: (x -> y -> y) -> y -> Constant a x -> y
    foldr    _    z   (Constant _)  = z

instance Traversable (Constant a) where 
    -- traverse :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)
    -- traverse ::                                   (a -> f b) -> t a -> f (t b)
    -- p1034: Lift A :: Applicative f             => (a -> b)   -> f a -> f b
    -- traverse  _  (Constant a)      = LiftA Constant (pure a)
    -- traverse ::                                   (x -> f y) -> Constant a x -> f (Constant a y)
    traverse  _  (Constant a)      = pure (Constant a)

instance (Eq a, Eq b) => EqProp (Constant a b) where (=-=) = eq
instance Arbitrary a => Arbitrary (Constant a b) where
    -- arbitrary :: Gen (Constant a b)
    -- Constant :: a -> Constant a b
    arbitrary = do Constant <$> arbitrary 
    -- more familiarly seen as 
    -- arbitrary = do
    --     a <- arbitrary
    --     return $ Constant a

testConstant = do
  quickBatch $ functor (undefined :: Constant (Int, Int, Int))
  quickBatch $ traversable (undefined :: Constant (Int, Int, [Int]))

-- 3. Maybe

data Optional a =  Nada
                |  Yep a  deriving (Eq, Ord, Show)

instance Functor Optional  where
    -- fmap :: (a -> b) -> Optional a -> Optional b
    fmap     _    Nada        = Nada
    fmap     f   (Yep x)      = Yep (f x)


instance Foldable Optional where
    -- foldMap :: Monoid m => (a -> m) -> t a -> m
    foldMap   _      Nada     = mempty
    foldMap   f     (Yep x)   = f x
    -- foldr :: (a -> b -> b) -> b -> t a -> b
    foldr     _  z   Nada     = z
    foldr     f  z  (Yep a)   = f a z

instance Traversable Optional where 
    -- traverse :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)
    -- traverse ::                                   (a -> f b) -> t a -> f (t b)
    -- traverse :: (Applicative f)                => (a -> f b) -> Optional a -> f (Optional b)
    --  p1034: liftA :: Applicative f             => (a -> b)   -> f a -> f b   
    -- fmap     :: Functor f                      => (a -> b)   -> f a -> f b
       traverse f     Nada     = pure Nada
       traverse f    (Yep a)   = fmap Yep (f a)  -- or liftA Yep (f a)  or Yep <$> f a

instance Eq a => EqProp (Optional a) where (=-=) = eq
instance Arbitrary a => Arbitrary (Optional a) where
  -- arbitrary :: Gen (Optional a)
  -- oneof :: [Gen a] -> Gen a
  -- Yep :: a -> Optional a
  arbitrary = oneof [ return Nada
                    , Yep <$> arbitrary]
    -- arbitrary = do Yep <$> arbitrary 
    -- -- more familiarly seen as 
    -- -- arbitrary = do
    -- --     a <- arbitrary
    -- --     return $ Yep a

-- 4. List
data List a = Nil
            | Cons a (List a) deriving (Eq, Show)

instance Functor List a where            
    -- fmap :: Functor f => (a -> b) -> f a    -> f b
    -- fmap ::              (a -> b) -> List a -> List b
    fmap     _   Nil            = Nil
    fmap     f   (Cons x xs)    = Cons (f x) (fmap f xs)

instance Foldable List where
    -- foldMap :: Monoid m => (a -> m) -> t a    -> m
    -- foldMap :: Monoid m => (a -> m) -> List a -> m
    foldMap   _  Nil           = mempty
    foldMap   f  (Cons x xs)   = f x `mappend` foldMap f xs

    -- foldr :: (a -> b -> b) -> b -> t a    -> b
    -- foldr :: (a -> b -> b) -> b -> List a -> b
    foldr     _  z  Nil         = Nil
    foldr     f  z  (Cons x xs) = f x (foldr f z xs)


instance Traversable List where -- i get quite lost on the <*> traverse
    -- traverse :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)
    -- traverse ::                                   (a -> f b) -> t a -> f (t b)
    -- traverse :: (Applicative f)                => (a -> f b) -> List a -> f (List b)
    -- Cons :: a -> List a -> List a   
    --  p1034: liftA2 :: Applicative f            => (a -> b -> c)   -> f a -> f b -> f c  
    -- fmap     :: Functor f                      => (a -> b)   -> f a -> f b
    -- fmap  :: (a -> (List a -> List a))  -> f List a -> f (List a -> List a)
    -- (<*>)  :: Applicative f                    => f (List a -> List b) -> List a -> List b

    traverse   _   Nil           = pure Nil
    traverse   f (Cons x xs)     = Cons fmap f x <*> traverse f xs
    -- traverse   f (Cons x xs)     = liftA2 Cons (f x) (traverse f xs)   -- alternative

instance Arbitrary a => Arbitrary (List a) where
    -- arbitrary :: Gen (List a)
    -- Cons :: a -> List a -> List a
    arbitrary = oneof [return Nil,
                       Cons <$> arbitrary <*> arbitrary]

instance Eq a => EqProp (List a) where 
    (=-=) = eq

-- 6. Three
data Three a b c =
  Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
    -- fmap :: Functor f => (a -> b) -> f a -> f b
    -- fmap :: (x -> y) -> Three a b x -> Three a b y
    fmap       f   (Three a b c)   = Three a b (f c)

instance Foldable (Three a b) where
    -- foldMap :: Monoid m => (a -> m) -> t a -> m
    foldMap f       (Three _ _ c)  = f c
    -- foldr :: (a -> b -> b) -> b -> t a -> b
    foldr   f   z   (Three _ _ c)  = f c z

instance Traversable (Three a b c) where
    -- traverse :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)
    -- fmap     :: Functor f                      => (a -> b)   -> f a -> f b    
    --  p1034: liftA :: Applicative f             => (a -> b)   -> f a -> f b   
    -- traverse :: Applicative f                  => (x -> f y) -> Three a b x -> f (Three a b y)
    -- Three :: a -> b -> c -> Three a b c
    -- fmap :: (y -> Three a b y) -> f y -> f (Three a b y)
    traverse f      (Three a b c)   = Three a b <$> f c -- or Three a b `fmap` f c   

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq


-- 7. Pair  
data Pair a b = Pair a b deriving (Eq, Show)

instance Functor (Pair a) where
    -- fmap :: Functor f => (a -> b) -> f a -> f b
    -- fmap :: (x -> y) -> Pair a x -> Pair a y
    fmap       f   (Pair a b)   = Pair a (f b)

instance Foldable (Pair a) where
    -- foldMap :: Monoid m => (a -> m) -> t a -> m
    -- foldMap :: Monoid m => (x -> m) -> (Pair a) x -> m
    foldMap f       (Pair _ b)  = f b

    -- foldr :: (a -> b -> b) -> b -> t a -> b
    -- foldr :: (x -> y -> y) -> y -> Pair a x -> y
    foldr   f   z   (Pair _ b)  = f b z

instance Traversable (Pair a) where
    -- traverse :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)
    -- traverse :: Applicative f                  => (x -> f y) -> Pair a x -> f (Pair a y)
    traverse  f (Pair a b) = Pair a <$> f b  

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
    arbitrary  = Pair <$> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Pair a b) where
  (=-=) = eq

-- 8. Big
-- When you have more than one value of type b, use Monoid and Applicative for the Foldable and Traversable instances, respectively:

data Big a b = Big a b b deriving (Eq, Show)

instance Functor (Big a) where
    -- fmap :: Functor f => (a -> b) -> f a -> f b
    -- fmap :: (x -> y) -> Big a x -> Big a y
    fmap   f   (Big a b1 b2)      = Big a (f b1) (f b2)

instance Foldable (Big a) where
    -- foldMap :: Monoid m => (a -> m) -> t a -> m
    -- foldMap :: Monoid m => (x -> m) -> (Big a) x -> m
    foldMap f       (Big _ b1 b2)  = f b1 <> f b2
    -- foldr :: (a -> b -> b) -> b -> t a -> b
    -- foldr :: (x -> y -> y) -> y -> Big a x -> y
    foldr   f   z   (Big _ b1 b2)  = (f b1. f b2) z

instance Traversable (Big a) where
    -- traverse :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)
    -- traverse :: Applicative f                  => (x -> f y) -> Big a x-> f (Big a y)
    traverse  f (Big a b1 b2) = Big a <$> f b1 <*> f b2

instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
    -- arbitrary :: Gen (Big a b)
    -- Big :: a -> b -> b -> Big a b
    arbitrary  = Big <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Big a b) where
  (=-=) = eq

-- 9. Bigger
data Bigger a b = Bigger a b b b deriving (Eq, Show)

instance Functor (Bigger a) where
    -- fmap :: Functor f => (a -> b) -> f a        -> f b
    -- fmap ::              (x -> y) -> Bigger a x -> Bigger a y
    fmap   f   (Bigger a b1 b2 b3)     = Bigger a (f b1) (f b2) (f b3)

instance Foldable (Bigger a) where
    -- foldMap :: Monoid m => (a -> m) -> t a         -> m
    -- foldMap :: Monoid m => (x -> m) -> (Bigger a) x -> m
    foldMap f  (Bigger _ b1 b2 b3)  = f b1 <> f b2 <> f b3
    -- foldr :: (a -> b -> b) -> b -> t a        -> b
    -- foldr :: (x -> y -> y) -> y -> Bigger a x -> y
    foldr   f   z (Bigger _ b1 b2 b3)  = (f b1. f b2 . f b3) z

instance Traversable (Bigger a) where
    -- traverse :: (Traversable t, Applicative f) => (a -> f b) -> t a        -> f (t b)
    -- traverse :: Applicative f                  => (x -> f y) -> Bigger a x -> f (Bigger a y)
    traverse  f (Bigger a b1 b2 b3) = Bigger a <$> f b1 <*> f b2 <*> f b3

instance (Arbitrary a, Arbitrary b) => Arbitrary (Bigger a b) where
    -- arbitrary :: Gen (Bigger a b)
    -- Bigger :: a -> b -> b -> b -> Bigger a b
    arbitrary  = Bigger <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Bigger a b) where
  (=-=) = eq

-- 10. S (took a leaf from JoHsi)
data S n a = S (n a) a deriving (Eq, Show)

instance ( Functor n
         , Arbitrary (n a)
         , Arbitrary a )
        => Arbitrary (S n a) where
  arbitrary = S <$> arbitrary <*> arbitrary

instance ( Applicative n
         , Testable (n Property)
         , Eq a
         , Eq (n a)
         , EqProp a )
        => EqProp (S n a) where
  (=-=) = eq

instance Functor n => Functor (S n) where
    -- fmap :: Functor f => (a -> b) -> f a   -> f b
    -- fmap ::              (a -> b) -> S n a -> S n b
    fmap f (S g x) = S (fmap f g) (f x)
    -- data S n a = S (n a) a
    -- S g x :: S n a   [[[ S :: n a -> a -> S n a]]]
    -- g :: n a, x :: a

instance Foldable n => Foldable (S n) where
    -- foldr :: (a -> b -> b) -> b -> t a   -> b
    -- foldr :: (a -> b -> b) -> b -> S n a -> b
    foldr f z (S p x) = f x (foldr f z p)
    -- p :: n a, x :: a
    -- foldr specialized to f and p :: (a -> b -> b) -> b -> n a -> b

    -- foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a  -> m with t ~ S n
    -- foldMap :: Monoid m => (a -> m) -> S n a -> m
    foldMap f (S p x) = foldMap f p <> f x
    -- p :: n a;  x :: a, f :: a -> m
    -- foldMap specialized to f and p :: (a -> m) -> n a -> m (This is my foldMap f p)
    -- (<>) :: Semigroup a => a -> a -> a 
    -- (a -> m) -> a -> m
    -- f           x -> f x  (This is my f x on right) and both are now type m so we can do <> and m has an instance of Semigroup.

instance Traversable n => Traversable (S n) where
    -- traverse :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)
    -- traverse :: Applicative f                  => (a -> f b) -> S n a -> f (S n b)
    traverse g (S p x) = S <$> traverse g p <*> g x

    -- p :: n a, x :: a; S p x :: S n a which is bcos S (n a) a = S n a; g :: (a -> f b)
    -- traverse specialized to f and p :: (a -> f b) -> n a -> f (n b)
    -- fmap :: Functor f => (a -> b) -> f a -> f b
    -- S :: n a -> a -> S n a
    -- fmap :: Functor f => (n a -> (a -> S n a)) -> f (n a)      -> f (a -> S n a)
    --                      S                        traverse g p   
    -- S <$> traverse g p :: f (a -> S n a) and g x :: f a
    -- (<*>) :: Applicative f => f (a -> b)     -> f a -> f b
    -- (<*>) :: Applicative f => f (a -> S n a) -> f a -> f (S n b)
  
testS = do
  quickBatch $ functor (undefined :: S Maybe (Int, Int, [Int]))
  --sample' (arbitrary :: Gen (S [] Int))
  quickBatch $ traversable (undefined :: S Maybe (Int, Int, [Int]))

-- 12. Tree
data Tree a =
    Empty
  | Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Eq, Show)

instance Functor Tree where
    -- fmap :: Functor f => (a -> b) -> f a   -> f b
    -- fmap ::              (a -> b) -> Tree a -> Tree b
    fmap    f      Empty                  = Empty
    fmap    f      (Leaf x)               = Leaf (f x)
    fmap    f      (Node left x right)    = Node (fmap f left) (f x) (fmap f right)

instance Foldable Tree where
    -- foldMap :: Monoid m => (a -> m)   -> t a    -> m  
    -- foldMap :: (Monoid m) => (a -> m) -> Tree a -> m
    foldMap  _      Empty                 = mempty                
    foldMap  f      (Leaf x)              = f x
    foldMap  f      (Node left x right)   = (foldMap f left) <> (f x) <> (foldMap f right)
    -- or as (foldMap f left) `mappend` (f x) `mappend` (foldMap f right)

    -- foldr :: (a -> b -> b) -> b -> t a        -> b
    -- foldr :: (x -> y -> y) -> y -> Tree a     -> y
    foldr   f   z   Empty                 = z
    foldr   f   z   (Leaf x)              = f x z
    foldr   f   z   (Node left x right)   = foldr f (f x (foldr f z right)) left

instance Foldable Tree where
    -- traverse :: (Traversable t, Applicative f) => (a -> f b) -> t a    -> f (t b)
    -- traverse :: Applicative f                  => (a -> f b) -> Tree a -> f (Tree b)
    --  p1034: liftA :: Applicative f             => (a -> b)   -> f a -> f b  
    --  p1125: liftA3 :: Applicative f            => (a -> b -> c -> d) -> f a -> f b -> f c -> f d 
    traverse _    Empty                 = pure Empty
    traverse f    (Leaf x)              = Leaf <$> f x  -- or liftA Leaf (f x)
    traverse f    (Node left x right)   = Node <$> traverse f left <*> f x <*> traverse f right  -- or liftA3 Node (traverse f left) (f x) (traverse f right)

instance Eq a => EqProp (Tree a) where
    (=-=) = eq

instance Arbitrary a => Arbitrary (Tree a) where 
    -- arbitrary :: Gen (Tree a) -- (ie f)
    -- Node :: Tree a -> a -> Tree a -> Tree a
    -- fmap :: Functor f => (a -> b) -> f a   -> f b
    -- fmap ::    (Tree a -> Node) -> Gen (Tree a) -> Gen (Node) 
    -- fmap :: (Tree a -> (a -> Tree a -> Tree a)) -> Gen (Tree a) -> Gen (a -> Tree a -> Tree a)
    --   arbitrary = oneof [ return Empty
    --                 , Leaf <$> arbitrary
    --                 , Node <$> arbitrary <*> arbitrary <*> arbitrary ]
     arbitrary = do
        a <- arbitrary
        l <- arbitrary
        r <- arbitrary
        frequency [(1, return Empty), (2, return $ Leaf a), (3, return $ Node l a r)]

testTree = do
  quickBatch $ functor (undefined :: Tree (Int, Int, Int))
  quickBatch $ traversable (undefined :: Tree ([Int], [Int], [Int]))