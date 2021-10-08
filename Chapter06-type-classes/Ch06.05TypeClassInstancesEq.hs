data Trivial = Trivial' -- Data is used to declare a type. Here the type is call Trivial

instance Eq Trivial where        -- declare Eq type class for the Trivial datatype
    Trivial' == Trivial' = True  -- how to test this datatype for equality

-- Pg 267
data DayOfWeek = 
    Mon | Tue | Weds | Thu | Fri | Sat | Sun
    

instance Eq DayOfWeek where
    -- Indent is impt
    (==) Mon Mon = True
    (==) Tue Tue = True
    (==) Weds Weds = True
    (==) Thu Thu = True
    (==) Fri Fri = True
    (==) Sat Sat = True
    (==) Sun Sun = True




--Pg 269
f :: Int -> Bool
f 1 = True
f 2 = True
f _ = False


data Identity a = Identity' a  -- Declare a type first

-- instance Eq (Identity a) where 
    -- (==) (Identity v) (Identity v') = v == v' -- error bcos altho v and v' are of type a but dunno a has Eq instances
    
    
--Soluition: ensure a has instance of Eq using Eq a => to create a class constraint , similarly as is done in functions
instance Eq a => Eq (Identity a) where   
    (==) (Identity' v) (Identity' v') = v == v'