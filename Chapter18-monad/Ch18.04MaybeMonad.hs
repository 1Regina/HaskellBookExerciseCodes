module MaybeMonad where

-- also check out https://github.com/sumidiot/haskellbook/tree/master/ch18
    
-- (>>=) :: Monad m
--         => m  a -> (a -> m  b)  ->  m  b
-- (>>=) ::  
--          Maybe a -> (a -> Maybe b) -> Maybe b


-- -- Same thing as pure:
-- return :: Monad m => a ->    m  a
-- return ::            a -> Maybe a


data Cow = Cow {
      name   :: String
    , age    :: Int
    , weight :: Int
    } deriving(Eq,Show)


noEmpty     :: String -> Maybe String
noEmpty ""  =  Nothing
noEmpty str = Just str

noNegative  :: Int -> Maybe Int
noNegative n | n >= 0    = Just n
             | otherwise = Nothing


-- if Cow's name is Bess,
-- it must be under 500

weightCheck :: Cow -> Maybe Cow 
weightCheck c = 
    let w = weight c 
        n = name c
    in if n == "Bess" && w > 499
            then Nothing
       else Just c 
                    
mkSphericalCow :: String -> Int -> Int -> Maybe Cow
mkSphericalCow name' age' weight' = 
    case noEmpty name' of
     Nothing    -> Nothing

     Just nammy -> 
         case noNegative age' of
            Nothing -> Nothing

            Just agey ->
                case noNegative weight' of 
                    Nothing -> Nothing
                    
                    Just weighty -> 
                        weightCheck
                            (Cow nammy agey weighty)

-- >>> mkSphericalCow "Bess" 5 499
-- Just (Cow {name = "Bess", age = 5, weight = 499})

-- >>> mkSphericalCow "Bess" 5 500
-- Nothing


mkSphericalCow' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow' name' age' weight' = do
    nammy   <- noEmpty name'
    agey    <- noNegative age' 
    weighty <- noNegative weight'
    weightCheck (Cow nammy agey weighty)

-- >>> mkSphericalCow' "Bess" 5 499
-- Just (Cow {name = "Bess", age = 5, weight = 499})


-- sequencing form >>= 

mkSphericalCow'' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow'' name' age' weight' = 
    noEmpty name' >>=
    \ nammy ->
        noNegative age' >>=
            \agey ->
                noNegative weight' >>=
                    \ weighty ->
                        weightCheck (Cow nammy agey weighty)

-- Cannot do Applicative.  Because our weightCheck function depends on the prior existence of a Cow value and returns more monadic structure in its return type, Maybe Cow.

-- All depends on how the do syntax looks. If like this: 
doSomething = do
    a <- f
    b <- g
    c <- h
    pure (a, b, c)
-- (ok to rewrite using Applicative)

-- BUT if it is like this: 
doSomething' n = do
    a <- f n
    b <- g a
    c <- h b
    pure (a, b, c)
-- ( cannot rewrite with Applicaive. Need Monad because g and h are producing monadic structure based on values that can only be obtained by depending on values generated from monadic structure. Need join to crunch the nesting of monadic structure back down)

-- Test above theory on need for join or >>= for it to work with do syntax. 

f   :: Integer -> Maybe Integer
f 0 = Nothing
f n = Just n

g   :: Integer -> Maybe Integer
g i = 
    if even i
    then Just (i + 1)
    else Nothing

h   :: Integer -> Maybe String
h i = Just ("10191" ++ show i)

--recall
-- doSomething' n = do
--     a <- f n
--     b <- g a
--     c <- h b
--     pure (a, b, c)

-- Summary: IMPT
-- 1.  With the Maybe Applicative, each Maybec omputation fails or succeeds independently of one another. You’re lifting functions that are also Just or Nothing over Maybe values.
-- 2.  With the Maybe Monad, computations contributing to the final result can choose to return Nothing based on previous computations.

-- See also beginning
-- (<$>) ::     Functor f => (a -> b)   ->   f a      -> f b
-- (<*>) :: Applicative f => f (a -> b) ->   f a      -> f b
-- (>>=) ::       Monad m =>    m a     -> (a -> m b) -> m b

-- per GHC Base
-- instance Monad Maybe where 
--     return x = Just x
    
--     (Just x) >>= k = k x
--     Nothing  >>= _ = Nothing


-- Recall above
-- mkSphericalCow'' :: String -> Int -> Int -> Maybe Cow
-- mkSphericalCow'' name' age' weight' = 
--     noEmpty name' >>=
--     \ nammy ->
--         noNegative age' >>=
--             \agey ->
--                 noNegative weight' >>=
--                     \ weighty ->
--                         weightCheck (Cow nammy agey weighty)

-- Therefore
-- mkSphericalCow'' "Bess" 5 499 = 
--     noEmpty "Bess" >>=
--     \ nammy ->
--         noNegative 5 >>=
--             \agey ->
--                 noNegative 499 >>=
--                     \ weighty ->
--                         weightCheck (Cow nammy agey weighty)


-- >>> noEmpty "Bess"
-- Just "Bess"

-- per GHC Base
-- instance Monad Maybe where 
--     return x = Just x
    
--     (Just x) >>= k = k x
--     Nothing  >>= _ = Nothing

-- noEmpty "Bess" >>= \nammy -> 
--     (restofthe computation)
-- noEmpty "Bess" evaluated
-- to Just "Bess". So the first
-- Just case matches.

--   (Just "Bess") >>= \nammy-> 
-- ...(Just x)     >>=  k      = k x
-- k is \nammy et al.
-- x is "Bess" by itself.

-- So nammy is bound to "Bess" , and the following is the whole k: (which is bound to noEmpty "Bess" by breakdown comparison? See mkSphericalCow'' )
-- \ "Bess" -> 
--     noNegative 5 >>=
--         \agey -> 
--             noNegative 499 >>=
--                 \weighty -> 
--                 weightCheck (Cow nammy agey weighty)

-- ageCheck. Recall
-- noNegative  :: Int -> Maybe Int
-- noNegative n | n >= 0    = Just n
--              | otherwise = Nothing
-- 5 >= 0 is true, so we get Just 5

-- noNegative 5 | 5 >= 0    = Just 5
--              |otherwise = Nothing


-- Again, although noNegative returns Just 5, the bind function >>= will pass 5 on:
-- mkSphericalCow'' "Bess" 5 499 =
--     noEmpty "Bess" >>=
--         \"Bess" -> 
--             noNegative 5 >>=
--                 \5 -> 
--                   noNegative 499 >>=
--                       \ weighty -> 
--                       weightCheck (Cow "Bess" 5 weighty)

-- 499 >= 0 is true, so we get  Just 499

-- noNegative 499 | 499 >= 0    = Just 499
--                |otherwise    = Nothing

-- Passing 499 on:
-- mkSphericalCow'' "Bess" 5 499 =
--     noEmpty "Bess" >>=
--         \"Bess" -> 
--             noNegative 5 >>=
--                 \5 -> 
--                   noNegative 499 >>=
--                       \ weighty -> 
--                       weightCheck (Cow "Bess" 5 weighty)

--Final weightCheck
-- weightCheck (Cow "Bess" 5 499) =
--     let 499    = weight (Cow "Bess" 5 499)
--         "Bess" = name   (Cow "Bess" 5 499)
-- -- fyi, 499 > 499 is False.
--     in if "Bess" == "Bess" && 499 > 499
--         then Nothing
--         else Just (Cow "Bess" 5 499)

-- So in the end, we return Just (Cow "Bess" 5 499)

-- Fail Scenario
-- noEmpty "" = Nothing
-- - noEmpty str = Just str
-- Nothing >>=
--     \nammy ->
--         -- Just case doesn't match, so skip it.
--         -- (Just x) >>= k= k x
--         -- This is what we're doing.
--         Nothing >>= _ = Nothing

-- >>= will drop the entire rest of the computation the moment any of the functions participating in the Maybe Monad actions produce a Nothing value.