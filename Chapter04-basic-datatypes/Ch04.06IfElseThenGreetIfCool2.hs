module GreetIfCool2 where

    -- not optimal but still pass
greetIfCool :: String -> IO ()
greetIfCool coolness = 
    if cool coolness
        then putStrLn "eyyyyy. What's shakin'?"
    else
        putStrLn"pshhhh."
    where cool v=
           v=="downright frosty yo"
-- >>> greetIfCool "downright frosty yo"
-- eyyyyy. What's shakin'?
--
-- >>> greetIfCool "hi "
-- pshhhh.
--

-- This is better
-- module GreetIfCool1 where 
greetIfCool1 :: String-> IO()
greetIfCool1 coolness = 
    if cool 
        then putStrLn "eyyyyy. What's shakin'?"
    else 
        putStrLn"pshhhh."
    where cool = 
           coolness =="downright frosty yo"
-- >>> greetIfCool1 "downright frosty yo"
-- eyyyyy. What's shakin'?
--
-- >>> greetIfCool1 "anything else"
-- pshhhh.
--