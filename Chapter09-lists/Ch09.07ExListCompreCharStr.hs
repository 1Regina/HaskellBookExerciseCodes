mySqr = [x^2 | x <- [1..10]]
-- >>> mySqr 
-- [1,4,9,16,25,36,49,64,81,100]
--

[ x | x <- mySqr , rem x 2 == 0]


-- Ans [4,16,36,64,100]


[(x, y) | x <- mySqr ,
          y <- mySqr , x<50 , y>50]

--Ans
--  =  [(x, y) | [1,4,9,16,25,36,49] , [64,81,100]
-- = [(1,64),(1,81),(1,100),(4,64),(4,81),(4,100),(9,64),(9,81),(9,100),(16,64),(16,81),(16,100),(25,64),(25,81),(25,100),(36,64),(36,81),(36,100),(49,64),(49,81),(49,100)]

take 5 [ (x, y) | x <- mySqr ,
                  y <- mySqr ,
                  x < 50, y > 50]

-- Ans [(1,64),(1,81),(1,100),(4,64),(4,81)]


-- ok in prelude
let acro xs = [x | x <- xs, 
               elem x ['A'..'Z']]                 
-- >>> acro "Three Letter Acronym"
-- (Error while loading modules for evaluation)

-- *Main Lib> :{
-- *Main Lib| [x |
-- *Main Lib| x <- "Three Letter Acronym",
-- *Main Lib|  elem x ['A'..'Z']]  
-- *Main Lib| :}
-- "TLA"

myString xs = [x | x <- xs, elem x "aeiou"]
-- -- take the string of words and return vowels