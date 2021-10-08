--Q1 
data Person = Person Bool deriving Show

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)
-- cannot type check bcos compiler cannot find implementation of type class Show for the type Person. Must insert deriving Show


--Q2
data Mood = Blah | Woot deriving (Eq, Show)

settleDown x = if x == Woot
                then Blah
               else x 
-- cannot type check bcos there is no Eq Mood instance in data Mood = Blah | Woot deriving Show. C
-- change to data Mood = Blah | Woot deriving (Eq, Show)

-- >>> settleDown Blah > Woot
-- <interactive>:3917:2-23: error:
--     * No instance for (Ord Mood) arising from a use of `>'
--     * In the expression: settleDown Blah > Woot
--       In an equation for `it': it = settleDown Blah > Woot

-- >>> settleDown Woot
-- Blah
--


-- --Q3
-- a) only Woot and Blah are acceptable values
-- b) error arise because Num  (for 9) has no instance for Mood
-- c) error arise bcos Mood instances (Blah and Woot) are not Ord instances

--Q4
type Subject = String
type Verb = String
type Object = String

data Sentence =  Sentence Subject Verb Object deriving (Eq, Show)

s1 = Sentence "dogs" "drool"
s2 = Sentence "Julie" "loves" "dogs"
-- >>> s2
-- Sentence "Julie" "loves" "dogs"
-- >>> s1 -- not enough arguments for Sentence
-- <interactive>:902:2-3: error:
--     • No instance for (Show (Object -> Sentence))
--         arising from a use of ‘print’
--         (maybe you haven't applied a function to enough arguments?)
--     • In a stmt of an interactive GHCi command: print it



-- s1 and s2 are currying with data constructor Sentence acting like a function
-- No instance for (Show (Object -> Sentence)) indicates that s1 is a function and there is no Show instances for functions see pg 318 footnote.
-- Explanation : https://stackoverflow.com/questions/39239577/partial-application-of-data-constructor



--Given a datatype declaration, what can we do?

-- Given the following datatype definitions:

data Rocks = Rocks String deriving (Eq, Show)

data Yeah = Yeah Bool deriving (Eq, Show)

data Papu = Papu Rocks Yeah deriving (Eq, Show)
-- to tyoecheck would need Papu (Rocks String) (Yeah Bool)
--Q1 - no
-- phew = Papu "chases" True -- wont type check bcos data constructors Rocks and Yeah are functions
phew = Papu (Rocks "chases") (Yeah True)
-- >>> phew
-- Papu (Rocks "chases") (Yeah True)
--

--Q2 - yes
truth = Papu (Rocks "chomskydoz")
             (Yeah True)


--Q3 -yes 
equalityForall :: Papu -> Papu -> Bool
equalityForall p p' = p == p'
--Papu has Eq instances

--Q4 -no bcos Eq is a superclass of Ord but not the other way round
-- comparePapus :: Papu -> Papu -> Bool
-- comparePapus p p' = p > p'
--No instance for (Ord Papu).

-- Match the types
--Q1a) 
-- i:: Num a => a
-- i = 1

-- Q1b 
i :: a
i = 1
-- >>> i
-- <interactive>:2075:2: error: Variable not in scope: i

 -- Cannot. No instance for (Num a) arising from the literal `1' bcos 1 is not parametrically polymorphic

-- Q2a
-- f :: Float
-- f = 1.0
-- --Q2b - Cannot
-- f1 :: Num a => a
-- f1 = 1.0 
-- Cannot type check as there is only Num Float, no Float Num. Float is a subclass of Num

-- -- Q3a 
-- f :: Float
-- f = 1.0

-- -- Q3b
-- f1 :: Fractional a => a
-- f1 = 1.0
-- Can type check as Fractional is polymorphic for 1.0


-- Q4a
f :: Float
f = 1.0

--Q4b
f1 :: RealFrac a => a
f1 = 1.0
-- Can bcos RealFrac is a subclass of Float so it can be be more narrowly defined.

--Q5a
freud :: a -> a
freud x = x

-- Qn5b
freud1 :: Ord a => a -> a
freud1 x = x
--Can bcos a is parametrically polymorphic

--Q6a
freud' :: a -> a
freud' x = x

--Q6b
freud'1 :: Int -> Int
freud'1 x = x
--Can bcos a is parametrically polymorphic

--Q7a
myX = 1 :: Int

sigmund :: Int -> Int
sigmund x = myX

-- Q7b
-- sigmund1 :: a -> a
-- sigmund1 x = myX
-- Cannot bcos myX is constrained type but a is polymorphic

--Q8a
myX' = 1 :: Int
sigmund' :: Int -> Int
sigmund' x' = myX'

--Q8b
-- sigmund'1 :: Num a => a -> a
-- sigmund'1 x' = myX'   
-- Cannot bcos result myX'  must be Int  which is stricter than Num. Solution: sigmund'1 :: Num a => a -> Int

--Q9a
jung :: Ord a => [a] -> a
jung xs = head (sort xs)

--Q9b
jung1 :: [Int] -> Int
jung1 xs = head (sort xs)
-- Can bcos Int can be ordered . Int has an Ord instance

-- Q10a
young :: [Char] -> Char
young xs = head (sort xs)

--Q10b
young1 :: Ord a => [a] -> a
young1 xs = head (sort xs)
-- Can Ord type allows for a to be sorted. young1 :: Ord a => [a] -> a makes this possible

--Q11a
mySort :: [Char] -> [Char]
mySort = sort

signifier :: [Char] -> Char
signifier xs = head (mySort xs)

-- Q11b
signifier' :: Ord a => [a]-> a
signifier' xs = head (mySort xs)
-- Can bcos strings has Ord instances[Wrong]
-- mySort takes a string and returns a string

--TYPE-KWON-DO TWO
--Q1
chk :: Eq b => (a -> b) -> a -> b -> Bool

chk f p q = f p == q 

--Q2 [Need help]
arith :: Num b => (a -> b) -> Integer -> a -> b
-- arith f x y = fromInteger(x) + f(y)
arith a2b _ a = a2b a      -- this is better