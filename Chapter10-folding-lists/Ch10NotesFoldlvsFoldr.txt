-- Rule 1
foldr f z xs =foldl (flip f) z (reverse xs)

Prelude Debug.SimpleReflect> mapM_ print $ reduction $ foldr (-) 5 [1,2,3,4] 
1 - (2 - (3 - (4 - 5)))
1 - (2 - (3 - (-1)))
1 - (2 - 4)
1 - (-2)
3

--without flip f
Prelude Debug.SimpleReflect> mapM_ print $ reduction $ foldl (-) 5 (reverse[1,2,3,4]) 
5 - 4 - 3 - 2 - 1
1 - 3 - 2 - 1
-2 - 2 - 1
-4 - 1
-5

-- so need flip f
Prelude Debug.SimpleReflect> mapM_ print $ reduction $ foldl (flip(-)) 5 (reverse[1,2,3,4]) 
1 - (2 - (3 - (4 - 5)))
1 - (2 - (3 - (-1)))
1 - (2 - 4)
1 - (-2)
3

foldr works with infinite lists
Prelude Debug.SimpleReflect> mapM_ print $ reduction $  foldr const 0 [1..]
1

foldl cannot work with inifinite lists

-- direct recursion, not using &&
myAnd::[Bool]-> Bool
myAnd []     = True 
myAnd (x:xs) = if x == False
               then False
               else myAnd xs

-- direct recursion, using &&
myAnd::[Bool]-> Bool
myAnd []     = True 
myAnd (x:xs) = not (x == False) && myAnd xs
will become x && myAnd xs

-- fold, not point-free
myAnd ::[Bool]-> Bool
myAnd = foldr(\a b-> if a== False
                     then False
                     else b)True


-- fold, both myAnd and the folding
-- function are point-free now
myAnd::[Bool]-> Bool
myAnd = foldr (&&) True


Tail Recursion. f is calling itself directly with no intermediaries.
foldr f z [] = z 
foldr f z (x:xs) = f x (foldr f z xs)


foldl f z [] = z
foldl f z (x:xs) = foldl f (f z x) xs