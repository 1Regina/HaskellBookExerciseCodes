avgGrade :: (Fractional a, Ord a) => a -> Char

avgGrade x 
    | y >= 0.9  = 'A'
    | y >= 0.8  = 'B'
    | y >= 0.7  = 'C'
    | y >= 0.59 = 'D'
    | y < 0.59 = 'F'
    where y = x / 100 -- where must always be last item
    

-- >>>  avgGrade 90
-- 'A'
--
-- >>> avgGrade 75
-- 'C'
--
-- >>> avgGrade 60
-- 'D'
--
avgGrade' x 
    | y >= 0.9  = 'A'
    | y >= 0.8  = 'B'
    | y >= 0.7  = 'C'
    | y >= 0.59 = 'D'
    | otherwise = 'F'
    where y = x / 100 -- where must always be last item

-- >>>  avgGrade' 90
-- 'A'
--
-- >>> avgGrade' 75
-- 'C'
--
-- >>> avgGrade' 60
-- 'D'
--
--Q1
avgGrade'' x 
    | otherwise = 'F'
    | y >= 0.9  = 'A'
    | y >= 0.8  = 'B'
    | y >= 0.7  = 'C'
    | y >= 0.59 = 'D'
    where y = x / 100 -- where must always be last item
-- It will always return F bcos restrictive ones should go at the top
-- >>>  avgGrade'' 90
-- 'F'
--
-- >>> avgGrade'' 75
-- 'F'
--
-- >>> avgGrade'' 60
-- 'F'
--
--Q2
-- No. 90 will not return a grade 'A' anymore. Scores from 70-90 will now be grade 'C'
avgGrade2 x 
    | y >= 0.7  = 'C' 
    | y >= 0.9  = 'A'
    | y >= 0.8  = 'B'

    | y >= 0.59 = 'D'
    | y < 0.59 = 'F'
    where y = x / 100 

-- >>> avgGrade2 90
-- 'C'
--
-- >>> avgGrade2 60
-- 'D'
--

--Q3-Q5
pal xs
    |xs == reverse xs = True
    |otherwise        = False

--Q3) b
--Q4) most  polymorphic form is Eq a -> [a] . It is a string and equality can be accepted
--Q5) pal :: Eq a => [a] -> Bool
-- >>> :t pal
-- pal :: Eq a => [a] -> Bool
--
--Q6-Q8
numbers x
  | x < 0  = -1
  | x == 0 = 0
  | x > 0  = 1

--Q6) c An indication of whether its argument is a positiveor negative number or 0.
--Q7) numbers can take all numbers but must be those which can be ordered so numbers :: (Num a, Ord a) => a

-- >>> numbers (-3.58)
-- -1

--Q8) 
-- >>> :t numbers
-- numbers :: (Ord a, Num a, Num p) => a -> p
--

