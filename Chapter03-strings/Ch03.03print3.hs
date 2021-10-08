module Print3 where

myGreeting :: String
-- myGreeting = "hello" ++ " world"
myGreeting = (++) "hello" " world!"

hello :: String
hello = "hello"

world :: String
world = "world!"

main :: IO ()
main = do
    putStrLn myGreeting
    putStrLn secondGreeting
    where
      secondGreeting = 
        -- concat [hello, " ", world]
        -- (++) "hello" ((++) " "  "world")
        -- hello ++ " " ++ world
        -- (++) hello ((++) " "  world)
        (++) "hello" ((++) " "  world)

-- area :: Int
area :: Floating a => a -> a
area d = pi * (r * r)
  where r = d/2

