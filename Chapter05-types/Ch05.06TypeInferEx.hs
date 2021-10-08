-- module Ch5Exercises where
-- Q1
-- (++) :: [a] -> [a] -> [a]
-- myConcat x = x ++" yo"
-- :t myConcat 
-- myConcat :: [Char] -> [Char]

-- Q2
-- (*) :: Num a => a -> a -> a
-- myMult x = (x / 3) * 5
-- :t myMult
-- myMult :: Fractional a => a -> a

-- Q3
-- take  :: Int -> [a] -> [a]
-- myTake x = take x "hey you"
-- :myTake
-- myTake :: Int -> [Char]
-- ie take an integer and returns a string

-- Q4
-- (>):: Ord a => a -> a -> Bool
-- myCom x = x > (length [1..10])
-- :t myCom
-- myCom :: Int -> Bool
-- ie put in an integer and returns a Bool


-- Q5
-- (<):: Ord a => a -> a -> BooL
-- myAlph x = x < 'z'
-- :t myAlph
-- myAlph :: Char -> Bool
-- ie put in a character and returns a Bool

--MCQ
-- Q1: A value of type [a] is               : c)   a list of elements that are all of some type a
-- Q2: A function of type[[a]] -> [a] could : a)   take a list of strings as an argument
-- Q3: A function of type [a] -> Int -> a   : b)   returns one element of type a from a list
-- Q4: A function of type (a, b) -> a       : c)   takes a tuple argument and returns the first value


--{-# LANGUAGE NoMonomorphismRestriction #-}

module DetermineTheType where
-- simple exampleexample=1
--Q1
-- a) (* 9) 6 :: Num a-> a ; Value = 54
-- b) head [(0,"doge"), (1,"kitteh")] produces :: Num a => (a, [Char]); Value = (0, "doge")
-- c) head [(0:: Integer,"doge"), (1,"kitteh")] produces :: (Integer, [Char]); Value = (0, "doge")
-- d) if False then True else False produces :: Bool; Value = False
-- e) length [1, 2, 3, 4, 5] produces :: Int ; Value = 5
-- f) (length [1, 2, 3, 4]) > (length "TACOCAT" ) produces :: Bool ;  Value = False

--Q2
x = 5
y = x + 5
w = y * 10
w :: Num a => a

--Q3
x = 5
y = x + 5
z y  = y * 10
z :: Num a => a -> a

--Q4
x = 5 
y = x + 5 
f = 4 / y
f :: Fractional a => a

--Q5
x = "Julie"
y = " <3 "
z = "Haskell"
f = x ++ y ++ z
f :: [Char]

--Does it Compile

--Q1
bigNum = (^) 5 $ 10
wahoo = bigNum $ 10
-- wahoo fails because function between bigNum and 10 is not expressed e.g. ++ ,  *
--fix
wahoo = (^) bigNum $ 10

--Q2
x = print 
y = print "woohoo!" 
z = x "hello world"

--Q3
a = (+)
b = 5 
c = b 10  -- No expression to describe c function 
Fix: c = a b 10 -- =  b + 10 
d = c 200 -- No expression to describe d function
Fix: d = a c 200-- c + 200 

--Q4
a = 12 + b
b = 10000 * c -- c is unknowns. There are undefined
    where c = 2 * a -- Fix


-- Variable or Specific Type Constructor
--Q2
f :: zed [1] -> Zed [2] -> Blah[3]
         [1] fully polymorphic -> [2] concrete -> [3] concrete bcos concrete are capitalised

--Q3
f :: Enum b [1] => a [2] -> b [3] -> C [4]
            [1] constrained polymorphic type variable -> [2] full polymorphic -> [3] constrained -> [4]concrete (bcos capitalised)
            [2] is full polymorphic bcos it is not another b

--Q4 
f :: f [1] -> g [2] -> C [3]
       [1] fully polymorphic -> [2] fully polymorphic -> [3] concrete bcos capitalised


-- Type Signature
--Q1
functionH :: [a] -> a
functionH (x :_)  = x

--Q2
functionC :: Ord a => a -> a -> Bool
functionC x y =   
    if (x > y) 
        then True 
    else False

--Q3
functionS :: (a, b) -> b
functionS (x, y) = y

--Example 
myFunc :: (x -> y) 
       -> (y -> z) 
       -> c 
       -> (a, x) 
       -> (a, z)

myFunc xToY yToZ _ (a, x) = (a , (yToZ (xToY x)))

--Q1
i :: a -> a
i = id a

--Q2 
c :: a -> b  -> a
c aei bee  = aei
     i) takes an agurment of type a
    ii) takes an argument of type b
   iii) finally returns an arugment of type a

--Q3
c'' :: b -> a -> b
c'' bee aei = bee
    i) takes an argument of type b
   ii) takes an arugment of type a
  iii) finally returns an arugment of type b

alpha equivalent if you can turn one into the other by renaming variables. Yes c and c'' are the same. 
c' aei bee = c'' bee aei

--Q4 ~ like take
c' :: a -> b -> b
c aei bee = bee
     i) takes an agurment of type a
    ii) takes an argument of type b
   iii) finally returns an arugment of type b

--Q5
r :: [a] -> [a]
r' x = reverse x
r'' x = x ++ "string"

--Q6
co :: (b -> c) --bToc takes an argument of type b and outputs an argument of type c
   -> (a -> b) --aTob takes an argument of type a and outputs an argument of type b
   -> a        --takes an argument of type a
   -> c        -- returns type c variable

co bToc aTob aei   = bToc (aTob aei) 


--Q7
a' :: (a -> c) --aToc takes an argument of type a and outputs an arugment of type c
   -> a        -- an argument of type a
   -> a        -- returns type a variable

-- a' aToc a  = aToc a -- incorrect
a' f a  = id a 
a' f = id

--Q8
a' :: (a -> b) -- aTob takes an argument of type a and outputs an argument of type b
    -> a       -- an argument of type a
    -> b       -- returns type b variable

a' aTob a = aTob a



