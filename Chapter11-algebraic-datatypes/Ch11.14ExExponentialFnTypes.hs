
-- Exponentiation in what order?

-- Yes, it holds. There are 2^3 or 8 implemetation
-- Note: this can help list the possible return value enumerations:
-- [(x, y, z) | x <- [True, False], y <- [True, False], z <- [True, False]]
data Quantum = Yes
             | No
             | Both
             deriving(Eq,Show)


convert1 :: Quantum -> Bool
convert1 Yes = True
convert1 No = True
convert1 Both = True

convert2 :: Quantum -> Bool
convert2 Yes = True
convert2 No = True
convert2 Both = False

convert3 :: Quantum -> Bool
convert3 Yes = True
convert3 No = False
convert3 Both = True

convert4 :: Quantum -> Bool
convert4 Yes = True
convert4 No = False
convert4 Both = False

convert5 :: Quantum -> Bool
convert5 Yes = False
convert5 No = True
convert5 Both = True

convert6 :: Quantum -> Bool
convert6 Yes = False
convert6 No = True
convert6 Both = False

convert7 :: Quantum -> Bool
convert7 Yes = False
convert7 No = False
convert7 Both = True

convert8 :: Quantum -> Bool
convert8 Yes = False
convert8 No = False
convert8 Both = False

-- The Quad--
data Quad = One
          | Two
          | Three
          | Four deriving(Eq,Show)
--Q1
-- eQuad :: Either Quad Quad 

-- eQuad has 4 + 4 = 8 inhabitants

--Q2
-- prodQuad::(Quad,Quad)
-- prodQuad has 4 ^ 2 = 16 inhabitants

--Q3.
-- funcQuad :: Quad-> Quad
-- funcQuad has 4^4 = 256 inhabitants
    -- >>> 4^4
    -- 256
    --

--Q4.
-- prodTBool::(Bool,Bool,Bool)
-- prodTBool has 2^3 = 8 inhabitants

--Q5.
-- gTwo:: Bool-> Bool-> Bool
-- gTwo has (2^2)^2 = 16 inhabitants
-- >>> (2^2)^2
-- 16
--

--Q6. Hint: five digit numberfTwo:: Bool-> Quad-> Quad
-- Bool-> Quad-> Quad has 65536 inhabitants
-- >>> (2^4)^4
-- 65536

