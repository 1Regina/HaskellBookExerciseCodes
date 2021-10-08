module MonadLaws where
-- import Data.Monoid ((<>))
-- import Control.Monad ((>=>))
-- import Control.Applicative

-- 1. IDENTITY LAWS
-- Monad has two identity laws:
-- right identity
m >>= return    = m

-- left identity
return (x) >>= f   = f x


(>>=):: Monad m 
    => m a -> (a -> m b) -> m b

-- First, right identity:
return :: a -> m a
m >>= return = m --return does not change function behaviour. like pure


--  left identity. return does not change function behaviour. like pure
return x >>= f = f  x

-- 2. ASSOCIATIVITY LAWS

(m >>= f) >>= g = m >>= (\x -> f x >>= g)
            --                 m a >>= g        First step -- See m a -> (a -> m b) -> m b
  
-- >>>  quickBatch (monad [(1, 2, 3)])
