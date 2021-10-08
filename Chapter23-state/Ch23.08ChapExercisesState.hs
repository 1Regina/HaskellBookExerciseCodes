# LANGUAGE InstanceSigs #-}

module ChapterExercises where

newtype State s a = State { runState :: s -> (a, s) }


instance Functor (State s) where
  -- we can write the specialized type signature here because of
  -- the LANGUAGE InstanceSigs pragma
  fmap :: (a -> b) -> State s a -> State s b
  fmap f (State g) = State $ \s -> let (a, s') = g s
                                   in  (f a, s')


instance Applicative (State s) where
  pure :: a -> State s a
  pure a = State $ \s -> (a, s)

  (<*>) :: State s (a -> b) -> State s a -> State s b
  (State f) <*> (State g) =
    -- we have f :: s -> (a -> b, s)
    --     and g :: s -> (a     , s)
    -- we want h :: s -> (     b, s)
    -- so, given s, we map it by f to get (ab, s'),
    --                 then apply g to s' to get (a, s'')
    --                 and return (ab a, s'')
    State $ \s -> let (ab, s') = f s
                      (a, s'') = g s'
                  in
                      (ab a, s'')


instance Monad (State s) where
  return = pure

  (>>=) :: State s a -> (a -> State s b) -> State s b
  (State f) >>= g =
    -- we have
    --   f :: s -> (a, s)
    --   g :: a -> (s -> (b, s))
    -- we want
    --   h :: s -> (b, s)
    -- so, given s
    --   apply f to get (a, s')
    --   then apply g to a and s' to get (b, s'')
    State $ \s -> let (a, s') = f s
                      m = g a
                  in
                      runState m s'


-- exercise 1
get :: State s s
get = State $ \s -> (s, s)
-- >>> runState get "curryIsAmaze"
-- ("curryIsAmaze","curryIsAmaze")


-- exercise 2
put :: s -> State s ()
put s = State $ \s' -> ((), s)
-- Alternatively
put s = State $ const ((), s)

-- >>>  runState (put "blah") "woot"


-- exercise 3
exec :: State s a -> s -> s
exec (State sa) s = snd $ sa s


-- exercise 4
eval :: State s a -> s -> a
eval (State sa) s = fst $ sa s


-- exercise 5
modify :: (s -> s) -> State s ()
modify f = State $ \s -> ((), f s)