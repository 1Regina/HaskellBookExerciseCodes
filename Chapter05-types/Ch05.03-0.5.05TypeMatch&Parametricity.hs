-- SECTION 5.3 Exercise
-- Q1 Functions
--a) mot :: Bool -> Bool
--b) length :: Foldable t => t a -> Int
--c) concat :: Foldable t => t [a] -> [a]
--d) head :: [a] -> a
--e) (<) :: a -> a -> Bool
-- Q2 Type Signatures
--a) head or tail :: [a] -> a 
--b)  concat :: [[a]] -> [a]
-- c) not :: Bool -> Bool
 --d) length :: [a] -> Int
 --e) (<) :: Ord a => a -> a -> Bool


--Section 5.4 Exercises
 --Type Arguments - tip give GHCi an undefined to bind the signature to
 -- u = undefined 
 -- f:: a-> a -> a -> a ; f = u 
 -- x :: Char; x = u
 -- :t f x
-- Q 1 If the type of f is a -> a -> a -> a, and the type of x is Char,then the type of f x :: Char -> Char -> Char
-- Q 2 If the type of g is a -> b -> c -> b, then the type of g 0 'c' "woot" is Char
-- Q 3 If the type of h is(Num a, Num b) => a -> b -> b, then the type of h 1.0 2 is Num b => b
-- Q 4 If the type of h is (Num a, Num b) => a -> b -> b, then the type of h 1 (5.5 :: Double) is Double
-- Q 5 If the type of jackal is (Ord a, Eq b) => a -> b -> a, then the type of jackal "keyboard" "has the word jackal in it" is [Char]
 --Q 6 If the type of jackal is (Ord a, Eq b) => a -> b -> a, then the type of jackal "keyboard" is Eq b => b -> [Char]
 --Q 7 If the type of kessel is (Ord a, Num b) => a -> b -> a, then the type of kessel 1 2 is (Ord a, Num a) => a
 --Q 8 If the type of kessel is (Ord a, Num b) => a -> b -> a, then the type of kessel 1 (2 :: Integer) is (Ord a, Num a) => a
 --Q 9 If the type of kessel is (Ord a, Num b) => a -> b -> a, then the type of kessel (1 :: Integer) 2 is Integer


-- Section 5.5 Polymorphism
 --Exercises: Parametricity
 -- Q1
pA :: a -> a
pA x = x 

-- Q2
pB :: a -> a -> a
pB x y = x
pB' x y = y


-- Q3
pC :: a -> b -> b
pC x y = y
-- Doesnt change behaviour



