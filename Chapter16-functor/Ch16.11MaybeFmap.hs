data Possibly a = LolNope
                | Yeppers a deriving(Eq,Show)
                
instance Functor Possibly where 
    fmap  _ LolNope  = LolNope
    fmap  f (Yeppers a) = Yeppers (f a) 

-- clue
-- applyIfJust :: (a->b) -> Maybe a -> Maybe b
