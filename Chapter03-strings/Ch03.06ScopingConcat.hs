-- print3Broken.hs
module Print3Broken where

printSecond :: IO ()
printSecond = do
  putStrLn greeting

main :: IO ()
main = do
  putStrLn greeting
  printSecond
--   where greeting = "Yarrrrr" -- to un break this and "Yarrrrr" twice on two different lines, remove where and put greeting at beginning of line

greeting :: [Char]
greeting = "Yarrrrr"
