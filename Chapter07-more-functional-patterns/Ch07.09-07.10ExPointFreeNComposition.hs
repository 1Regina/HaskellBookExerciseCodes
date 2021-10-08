module Arith2 where

add :: Int -> Int -> Int
add x y = x + y

addPF :: Int -> Int -> Int
addPF = (+)

addOne :: Int -> Int
addOne = \x -> x + 1

addOnePF :: Int -> Int
addOnePF = (+ 1)

main :: IO ()
main = do 
  print (0 :: Int) -- 0 
  -- >>> print (0 :: Int)
  -- 0
  --
  print (add 1 0) -- 1 + 0 = 1
  -- >>> print (add 1 0) 
  -- 1
  --
  print (addOne 0) -- 0 + 1 = 1
  -- >>> addOnePF (0)
  -- 1
  --
  print (addOnePF 0) -- (+1) 0 = 1
  -- >>> addOnePF 0
  -- 1
  --
  print ((addOne . addOne) 0) -- addOne (addOne 0) = addOne (0 + 1) = (0 + 1) + 1 = 2
  -- >>> print ((addOne . addOne) 0) 
  -- 2
  --
  print ((addOnePF . addOne) 0) -- addOnePF (addOne 0) = addOnePF (0 + 1) = addOnePF (1) = (+1) (1) = 2
  -- >>> print ((addOnePF . addOne) 0)
  -- 2
  --
  print ((addOne . addOnePF) 0) -- addOne (adOnePF 0) = addOne ((+1) 0)) = addOne (1) = 1 + 1 = 2
  -- >>> print ((addOne . addOnePF) 0)
  -- 2
  --
  print ((addOnePF . addOnePF) 0) -- addOnePF (addOnePF 0) = addOnePF ((+1) 0) = addOnePF (1) = (+1) (1) = 2
  -- >>>  print ((addOnePF . addOnePF) 0)
  -- 2
  --
  print (negate (addOne 0)) -- negate (0 + 1) = negate 1 = = -1  
  -- >>> print (negate (addOne 0)) 
  -- -1
  --
  print ((negate . addOne) 0) -- negate (addOne 0) = negate (0 + 1) = negate 1 = -1
  -- >>> print ((negate . addOne) 0) 
  -- -1
  --
  print ((addOne . addOne . addOne . negate . addOne) 0) -- addOne . addOne . addOne . negate  (addOne 0) = addOne . addOne . addOne . negate ( 0 + 1) = addOne . addOne . addOne (-1) = addOne . addOne ((-1) +1) = addOne . addOne (0) = addOne ( 0 + 1) = addOne 1 = 1 + 1 = 2
  -- >>> print ((addOne . addOne . addOne . negate . addOne) 0) 
  -- 2
  --
