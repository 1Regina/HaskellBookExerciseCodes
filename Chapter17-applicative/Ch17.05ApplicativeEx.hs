import Applicative
-- use <$> from Functor and <*> and pure from the Applicative type class

-- Q1
-- v = const <$> Just "Hello" <*> pure "World"
-- >>> const <$> Just "Hello" <*> pure "World"
-- Just "Hello"
-- >>> v
-- Just "Hello"
--


-- Q2

(,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1,2,3]
-- Just (90,10,"Tierness",[1,2,3])
