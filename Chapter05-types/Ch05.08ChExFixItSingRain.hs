module Sing where

fstString :: [Char] -> [Char]
fstString x = x ++ " in the rain"

sndString :: [Char] -> [Char]
sndString x = x ++ " over the rainbow"

sing :: Ord a => a -> a -> [Char]
sing a b  = if a < b 
                then fstString x 
            else 
                sndString y
            where x = "Singin" ;
                  y = "Somewhere"
                  
-- >>> sing 1 3
-- "Singin in the rain"
--- >>> sing 'p' 'j'
--- "Somewhere over the rainbow"
--- >>> sing "mary" "jane"
--- "Somewhere over the rainbow"
--- >>> sing ["mary"] ["jane"]
--- "Somewhere over the rainbow"
---
