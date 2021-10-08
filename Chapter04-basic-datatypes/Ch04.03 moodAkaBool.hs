
data Mood = Blah | Woot deriving Show

changeMood :: Mood-> Mood
changeMood Blah = Woot
changeMood Woot = Blah


-- >>> not True && True -- Capitalized the 2nd True
-- False
-- >>> not (x == 6) -- add one more = sign but x not in scope
-- <interactive>:568:7: error: Variable not in scope: x :: Integer
-- >>> (1 * 2) > 5  -- ok
-- False
-- >>> ["Merry"] > ["Happy"] -- add  " "
-- True
-- >>> "Merry" > "Happy" 
-- True
-- >>> [1, 2, 3] ++ "look at me!" -- cannoot concat diff types so must change both to strings
--- >>> ["1", "2", "3"] ++ ["look at me!"]
--- ["1","2","3","look at me!"]
---


