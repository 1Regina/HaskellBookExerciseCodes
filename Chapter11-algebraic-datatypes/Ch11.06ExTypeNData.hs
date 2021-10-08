data Price              = Price Integer deriving (Eq, Show)

data Manufacturer       = Mini 
                        | Mazada 
                        | Tata 
                        deriving (Eq, Show)

data Airline            = PapuAir 
                        | CatapultsR'Us
                        | TakeYourChancesUnited
                        deriving (Eq, Show)

data Vehible            = Car Manufacturer Price
                        | Plane Airline
                        deriving (Eq, Show)

myCar                   = Car   Mini    (Price  14000)
urCar                   = Car   Mazda   (Price  20000)
clownCar                = Car   Tata    (Price  7000)
doge                    = Plane PapuAir

--Q1
-- >>> :t myCar
-- myCar :: Vehible
--
--Q2
isCar :: Vehicle-> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle-> Bool
isPlane (Plane _ ) = True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

--Q3
getManu :: Vehicle -> Manufacturer
getManu (Car m _ ) = m
getManu _ = error "only cars have manufacturers"

--Q4
-- 4) It will raise a runtime error (bottom). The return type should
--    be changed to Maybe Manufacturer so we can return Nothing instead.

-- >>> getManu (Car Mini (Price  14000)) 


-- 5) Updated types and functions:

data Size = Capacity Integer
          deriving (Eq, Show)

data Vehicle = Car Manufacturer Price
             | Plane Airline Size
             deriving (Eq, Show)

isPlane :: Vehicle-> Bool
isPlane (Plane _ _ ) = True
isPlane _ = False
