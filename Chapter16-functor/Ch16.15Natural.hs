{-# LANGUAGE RankNTypes #-}
type Nat f g = forall a. f a -> g a
-- Change structure but leave input arguments alone

-- Prelude> :set -XRankNTypes
-- Prelude> :{
-- *Main| type Nat f g =
-- *Main|      forall a . f a -> g a
-- *Main| :}
-- Prelude>

-- To see an example of what the quantification prevents, consider the following:

type Natf g = forall a.f a -> g a
-- This'll work
maybeToList :: Nat Maybe []
maybeToList Nothing = []
maybeToList (Just a) = [a]
