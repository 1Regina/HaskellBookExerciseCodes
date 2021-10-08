-- Not a good way to write but still works
class Numberish a where
 fromNumber :: Integer -> a
 toNumber :: a -> Integer

newtype Age = Age Integer deriving (Eq, Show)

instance Numberish Age where
    fromNumber n = Age n
    toNumber (Age n) = n

newtype Year =  Year Integer deriving (Eq, Show)

instance Numberish Year where
    fromNumber n = Year n
    toNumber (Year n) = n

sumNumberish :: Numberish a => a -> a -> a 
sumNumberish a a' = fromNumber summed
 where integerOfA = toNumber a
       integerOfAPrime = toNumber a'
       summed = integerOfA + integerOfAPrime

-- >>> sumNumberish (Age 10) (Age 10)
-- Age 20
-- >>> sumNumberish (Year 10) (Year 10)
-- Year 20
--
add :: Num a => a -> a -> a
add x y = x + y
-- >>> add 1 2
-- 3
--
addWeird :: (Ord a, Num a) => a -> a -> a
addWeird x y = 
    if x > 1 
        then x + y
    else x

addAlso :: Int -> Int -> Int
addAlso x y = x + y

addWeirdAlso :: Int -> Int -> Int
addWeirdAlso x y = 
    if x > 1
        then x + y
    else x

check'Also :: Int -> Int -> Bool
check'Also a a' = a == a'
