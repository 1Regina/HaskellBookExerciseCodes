bind :: Monad m => (a -> m b) -> m a -> m b
-- replace f with m
-- >>> :t fmap
-- fmap :: Functor f => (a -> b) -> f a -> f b
-- fmap :: Functor m => (a -> b) -> m a -> m b

-- Replace a with m
-- >>> :t join
-- join :: Monad a => a (a b) -> a b
-- join :: Monad m => m (m b) -> m b
-- join :: Monad m => m (m a) -> m a

bind = join .fmap

Control.Monad.join :: Monad m => m (m a) -> m a