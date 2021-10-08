-- P1130 WHY WHEN FMAP is not enough
<$> :: Functor f => (a->b) -> f a ->f b
-- putStrLn:: String-> IO()
putStrLn :: String-> IO()

f :: Functor f => f String -> f (IO())
f x = putStrLn <$> x

g :: (String -> b) -> IO b
g x = x <$> getLine

putStrLn <$> getLine :: IO(IO())

h :: IO(IO())
h = putStrLn <$> getLine


-- Cannot fmap putStrLn over getLine. need join
-- join does here is merge the effects of getLine and putStrLn into a single IO action. This merged IO action performs the effects in the order determined by the nesting of the IO actions.

-- Prelude> import Control.Monad (join)
-- Prelude> join $ putStrLn <$> getLine
-- Prelude> :t join $ putStrLn <$> getLine
-- join $ putStrLn <$> getLine :: IO ()

-- monadic actions are still pure, and the sequencing operations we use here are ways of nesting lambdas

bindingAndSequencing :: IO ()
bindingAndSequencing = do 
        putStrLn "name pls:" 
        name <- getLine
        putStrLn ("y helo thar: "++ name)

bindingAndSequencing' :: IO ()
bindingAndSequencing' = 
    putStrLn "name pls:" >>
    getLine >>=
    \name -> 
        putStrLn ("y helo thar: "++ name)


twoBinds :: IO ()
twoBinds = do 
    putStrLn "name pls:" 
    name <- getLine
    
    putStrLn "age pls:"
    age <- getLine
    putStrLn ("y helo thar: "
              ++ name ++ " who is: "
              ++ age ++" years old.")

twoBinds' :: IO ()
twoBinds' = 
    putStrLn "name pls:" >>
     getLine >>=
         
      \name -> 
        putStrLn "age pls:" >>
         getLine >>=
             
            \age -> 
             putStrLn ("y helo thar: "
                        ++ name ++ " who is: "
                        ++ age ++" years old.")

