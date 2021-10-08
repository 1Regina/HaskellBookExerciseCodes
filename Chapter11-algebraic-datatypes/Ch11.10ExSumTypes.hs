-- Exercises: Pity the Bool

-- 1)   Big Bool | Small Bool
--    = Big (2)  + Small (2)
--    Big and Small are unary data constructors, so they have the cardinality
--    of the type they contain, which for Bool is 2
--    = 2 + 2
--    = 4

-- Q2) cardinality of NumberOrBool
-- NumberOrBool = Numba Int8 | BoolyBool Bool
--              = (2^7 * 2)  +  2
--              =  256       +  2
--              =  258

import Data.Int ( Int8 ) 
data NumberOrBool = Numba Int8 | BoolyBool Bool deriving (Eq,Show)

myNumba :: NumberOrBool
myNumba = Numba(-128)   

-- >>> Numba1 = Numba (-12)
-- <interactive>:1005:2-7: error:
--     Not in scope: data constructor ‘Numba1’
--     Perhaps you meant ‘Numba’ (line 17)
--
-- n = Numba (3229)
-- warning: [-Woverflowed-literals]
--     Literal 3229 is out of the Int8 range -128..127

-- n = Numba (229)
-- warning: [-Woverflowed-literals]
--     Literal 229 is out of the Int8 range -128..127
