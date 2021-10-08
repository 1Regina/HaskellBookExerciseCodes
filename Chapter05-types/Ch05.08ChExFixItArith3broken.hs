module Arith3Broken where

main :: IO ()
main = do
    print ((1 + 2) ::Integer )
    putStrLn "10"
    print (negate (-1) :: Integer)
    print ((+) 0 (blah)) 
      where blah = negate 1

-- ALTERNATIVE WITHOUT the :: Integer
main1 :: IO ()
main1 = do
  print (1 + 2)
  putStrLn "10"
  print (negate 1)
  print ((+) 0 blah)
    where blah = negate 1    