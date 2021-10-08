{-# LANGUAGE FlexibleInstances #-}

-- import GHC.Arr (Array)

module WorkingFunctor where

-- Q1
data Sum b a = First a | Second b -- flipped a and b  so first argument is ignored
instance Functor (Sum a) where
  fmap f (First a)  = First (f a)
  fmap _ (Second b) = Second b


-- Q2
data Company a c b = DeepBlue a c | Something b -- moved b to end bcos u want only the b to be effected
instance Functor (Company e e') where
  fmap f (Something b)  = Something (f b)
  fmap _ (DeepBlue a c) = DeepBlue a c


-- Q3
data More b a = L a b a | R b a b deriving (Eq, Show) -- flipped a and b
instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'

lp1 = fmap (+1) (L 1 2 3) -- should be L 2 2 4
rp1 = fmap (+1) (R 1 2 3) -- should be R 1 3 3

-- >>> lp1
-- >>> rp1
-- L 2 2 4
-- R 1 3 3
--


-- p1022
-- Q1

data Quant a b = Finance | Desk a | Bloor b deriving Show

--standard one where only the second argument gets applied to f while first agrument is bound
instance Functor (Quant a) where
  fmap _ Finance   = Finance
  fmap _ (Desk a)  = Desk a
  fmap f (Bloor b) = Bloor (f b)


-- Q2
data K a b = K a deriving Show

instance Functor (K a) where
  fmap _ (K a) = K a


-- Q3
newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

newtype K' a b = K' a deriving Show -- ie b is unimpt

instance Functor (Flip K' a) where
  fmap f (Flip (K' a)) = Flip (K' (f a)) -- Bcos b is unimpt

-- Q4
data EvilGoateeConst a b = GoatyConst b deriving Show

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst (f b)

-- Q5
data LiftItOut f a = LiftItOut (f a) deriving Show  

instance Functor f => Functor (LiftItOut f) where
  fmap g (LiftItOut fa) = LiftItOut (fmap g fa)

-- Q6
data Parappa f g a = DaWrappa (f a) (g a) deriving Show  

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap h (DaWrappa fa ga) = DaWrappa (fmap h fa) (fmap h ga)

-- >>> fmap (+1) (DaWrappa (Just 6) (Nothing))
-- DaWrappa (Just 7) Nothing
-- >>> fmap (+1) (DaWrappa (Just 6) (Left "err"))
-- DaWrappa (Just 7) (Left "err")
-- >>> fmap (+1) (DaWrappa (Just 6) (Right 10.8))
-- DaWrappa (Just 7.0) (Right 11.8)

-- Q7
data IgnoreOne f g a b = IgnoreSomething (f a) (g b) deriving Show

instance (Functor f, Functor g) => Functor (IgnoreOne f g a) where
  fmap h (IgnoreSomething fa gb) = IgnoreSomething fa (fmap h gb)

-- >>> fmap (+1) (IgnoreSomething (Just 6) (Right 10.8))
-- IgnoreSomething (Just 6) (Right 11.8)
--

-- Q8
data Notorious g o a t = Notorious (g o) (g a) (g t) deriving Show

instance Functor g => Functor (Notorious g o a) where
  fmap h (Notorious go ga gt) = Notorious go ga (fmap h gt)


-- Q9  

data List a = Nil | Cons a (List a) deriving Show

instance Functor List where
  fmap _ Nil        = Nil
  fmap f (Cons a l) = Cons (f a) (fmap f l)

--    > fmap (+1) (Cons 2 (Cons 1 (Cons 0 Nil)))
--    Cons 3 (Cons 2 (Cons 1 Nil))

-- Q10
data GoatLord a = NoGoat
                | OneGoat a
                | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
                deriving Show

instance Functor GoatLord where
  fmap _ NoGoat               = NoGoat
  fmap f (OneGoat a)          = OneGoat (f a)
  fmap f (MoreGoats g1 g2 g3) = MoreGoats (fmap f g1) (fmap f g2) (fmap f g3)


-- Q11
data TalkToMe a = Halt
                | Print String a
                | Read (String -> a)

instance Functor TalkToMe where
  fmap _ Halt         = Halt
  fmap f (Print s a)  = Print s (f a)
  fmap f (Read sa) = Read (fmap f sa)
 
 -- alternatively...
 -- fmap f (Read sa)    = Read (\s -> f (sa s))