module Hello 
    (sayHello) -- Ch13.05 Module exports
    where

-- Ch13.04 Module exports
-- sayHello:: IO()
-- sayHello = do
--     putStrLn "hello world"


-- Ch13.07 - make program interactive
sayHello :: String-> IO ()
sayHello name = 
    putStrLn ("Hi "++ name ++ "!")