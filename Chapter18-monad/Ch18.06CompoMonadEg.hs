import Control.Monad ((>=>))
sayHi :: String -> IO String
sayHi greeting = do
    putStrLn greeting
    getLine

readM :: Read a => String -> IO a
readM = return .read

getAge :: String -> IO Int
getAge = sayHi >=> readM

askForAge :: IO Int
askForAge = getAge "Hello! How old are you?"     

-- Kleisli composition operator to stitch sayHi and readM together

--     [1]  [2][3] 
--     (a -> m  b) 
-- String -> IO String

--      [4]  [5][6]
-- ->   (b -> m  c) 
--  String -> IO a

--      [7]  [8][9]
-- ->    a -> m c
--  String -> IO a

-- 1. The first type is the type of the input tosayHi,String.
-- 2. The IO that sayHi performs in order to present a greeting and receive input.
-- 3. The String input from the user that sayHi returns.
-- 4. The String that readM expects as an argument and that sayHi will produce.
-- 5. The IO readM returns into. Note that return/pure produce IO values that perform no I/O.
-- 6. The Int that readM returns.
-- 7. The original, initial String input that sayHi expects so it knows how to greet the user and ask for their age.
-- 8. The final combined IO action that performs all effects necessary to produce the final result.
-- 9. The value inside of the final IO action; in this case, this is the Int value that readM returns.