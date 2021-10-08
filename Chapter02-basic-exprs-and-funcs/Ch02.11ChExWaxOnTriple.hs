-- module NewMult1 where


-- mult1 = x * y
--  where x = 5 ;  y = 6

-- module Exercise1 where
-- triplePlus = x * 3 + y 
--      where 
--          x = 3
--          y = 1000

-- module Exercise2 where
--  times5 = x * 5
--         where
--             y = 10
--             x = 10 * 5 + y

-- module Exercise3 where
--     divAndPlus = z / x + y 
--         where
--             x = 7
--             y = negate x
--             z = y * 10

module TripleWaxOn where
waxOn :: Integer  
waxOn = x * 5
    where  
        z = 7 
        y = z + 8
        x = y ^ 2

triple :: Num a => a -> a
triple x = x * 3

waxOff :: Num a => a -> a
waxOff = triple  

waxOffSq :: Num a => a -> a
waxOffSq x =  (^) (triple x) 2  