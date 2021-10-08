module Main where

-- main :: IO ()
-- main = do
--   putStrLn "hello world"

-- Ch13.05
-- import DogsRule
-- import Hello
-- main :: IO ()
-- main = do
--   sayHello
--   dogs

-- Ch13.07 Make interactive
-- import DogsRule
-- import Hello
-- main:: IO()
-- main= do
--   name <- getLine
--   sayHello name
--   dogs  -- in DogsRule dogs module

import DogsRule
import Hello
import System.IO

main:: IO()
main = do
  hSetBuffering stdout NoBuffering
  putStr"Please input your name: "
  name <- getLine
  sayHello name
  dogs