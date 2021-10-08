-- divideThenAdd x y = (x / y) + 1
-- divideThenAdd :: Num a => a -> a -> a
-- divideThenAdd x y = (x / y ) + 1

-- error bcos / operator is for Fractional and not Num type . Correct this by 
divideThenAdd :: Fractional a => a -> a -> a
divideThenAdd x y = (x / y ) + 1


-- >>> divideThenAdd 10 5
-- 3.0
--
--Thinking Cap : 
f ::(Num a, Fractional a) => a -> a -> a --works bcos Fractional is a subclass of Num
f x y  = (x / y) + 1
-- >>> f 6 2
-- 4.0
--

numId = id :: Num a => a -> a
intId = numId :: Int -> Int
numId' = intId  :: Num a =>  a ->  a --wont work bcos actual type is more concrete (Int) than expected type (Num)

-- >>> numId 4
-- 4
-- >>> intId 4
-- 4
--
-- >>> numId' 4 --mismatch error between expected and actual type!
-- *** Exception: /Users/regina/Code/Haskell/HaskellBook/HaskellBookOtherChs/src/Ch06.06NumTypeClasses.hs:22:10-14: error:
--     • Couldn't match type ‘a’ with ‘Int’
--       ‘a’ is a rigid type variable bound by
--         an expression type signature:
--           forall a. Num a => a -> a
--         at /Users/regina/Code/Haskell/HaskellBook/HaskellBookOtherChs/src/Ch06.06NumTypeClasses.hs:22:20-36
--       Expected type: a -> a
--         Actual type: Int -> Int
--     • In the expression: intId :: Num a => a -> a
--       In an equation for ‘numId'’: numId' = intId :: Num a => a -> a
-- (deferred type error)
--


