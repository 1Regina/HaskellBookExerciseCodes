myNum :: Integer
myNum = 1

myVal = myNum

-- >>> :t myVal
-- myVal :: Integer
--

myVal1 f = myNum
-- >>> :t myVal1
-- myVal1 :: p -> Integer
--
-- type p is polymorphic and f is maximally polymorphic
-- 
-- myNum = 1:: Integer
myVal2 f = f + myNum
-- >>> :t myVal2
-- myVal2 :: Integer -> Integer

myNum' = 1 :: Integer
myVal3 f g = myNum'
-- :t myVal3 should be myVal3 :: t -> t1 -> Integer
-- >>> :t myNum'
-- myNum' :: Integer
-- >>> :t myVal3
-- myVal3 :: p1 -> p2 -> Integer
--

myVal4 f g h = myNum
-- >>> :t myVal4
-- myVal4 :: p1 -> p2 -> p3 -> Integer
--


bindExp :: Integer-> String
bindExp x = 
    let y = 5 in
        -- let z = y + x in 
             "the integer was: " ++ show x 
               ++ " and y was: " ++ show y
            --    ++ " and z was: " ++ show z
-- >>> bindExp 7
-- "the integer was: 7 and y was: 5"
--
bindExp1 :: Integer-> String
bindExp1 x = 
    let y = 5 in
        let z = y + x in 
             "the integer was: " ++ show x 
               ++ " and y was: " ++ show y
               ++ " and z was: " ++ show z
-- >>> bindExp1 7
-- "the integer was: 7 and y was: 5 and z was: 12"
--
