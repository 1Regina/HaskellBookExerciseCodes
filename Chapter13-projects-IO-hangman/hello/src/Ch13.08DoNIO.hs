twoo:: IO Bool
twoo =  do c <- getChar
           c'<- getChar
           c == c' 

-- >>> twoo
-- *** Exception: /Users/regina/HaskellProj/hello/src/Ch13.08DoNIO.hs:4:12-18: error:
--     • Couldn't match expected type ‘IO Bool’ with actual type ‘Bool’
--     • In a stmt of a 'do' block: c == c'
--       In the expression:
--         do c <- getChar
--            c' <- getChar
--            c == c'
--       In an equation for ‘twoo’:
--           twoo
--             = do c <- getChar
--                  c' <- getChar
--                  c == c'
-- (deferred type error)
--
twoo1 :: IO Bool
twoo1 =  do c  <- getChar
            c' <- getChar
            return (c == c') 

-- >>> twoo1

main:: IO()
main = do c <- getChar
          c'<- getChar
          if c==c'
            then putStrLn "True"
            else return()

-- >>> main

