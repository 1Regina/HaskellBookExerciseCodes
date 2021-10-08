-- Not able to check in GHCI
--Q1
f :: Int-> String
f = undefined -- int2string

g :: String-> Char
g = undefined -- string2char

h :: Int-> Char
h x = g (f x) 


--Q2
data A
data B
data C 

q :: A -> B
q = undefined

w :: B -> C
w = undefined

e :: A -> C
e = w.q 

--Q3
data X
data Y 
data Z

xz :: X -> Z
xz = undefined

yz :: Y -> Z
yz = undefined

xform::(X,Y) -> (Z,Z)
xform (x, y) = (xz x, yz y)

--Q4

munge :: (x -> y)           -- x2y take an argument of type x and outputs an argument of type y
      -> (y -> (w, z))      -- y2wz takes an argument of type y and outputs a tuple comprising argument of types w and z respectively
      -> x                  -- an argument of type x
      -> w                  -- returns type w variable

munge x2y y2wz x' = fst.y2wz $ x2y x' -- = fst $ ywz $ xy x