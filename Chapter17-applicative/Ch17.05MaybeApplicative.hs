module Maybee where 
validateLength :: Int 
               -> String
               -> Maybe String

validateLength maxLen s = 
    if (length s) > maxLen
    then Nothing
    else Just s


newtype Name = Name String deriving (Eq,Show)
newtype Address = Address String deriving (Eq,Show)

mkName :: String -> Maybe Name
mkName s = fmap Name $ validateLength 25 s 

mkAddress :: String -> Maybe Address 
mkAddress a = fmap Address $ validateLength 100 a

data Person = Person Name Address deriving (Eq,Show)
-- mkPerson :: Maybe Name -> Maybe Address -> Maybe Person
-- mkPerson :: Name -> Address -> Maybe Person
mkPerson :: String -> String -> Maybe Person -- something wrong here with no binding

mkPerson n a = 
    case mkName n of
        Nothing -> Nothing
        Just n' -> 
            case mkAddress a of
                     Nothing -> Nothing
                     Just a' -> Just $ Person n' a'


-- >>> :t fmap Person (mkName "Babe")
-- fmap Person (mkName "Babe") :: Maybe (Address -> Person)
--

-- Need to use applicative to map function embedded in our f to overcome (a->b) inside Maybe

addy = mkAddress "old macdonald's"
b = mkName "Babe"
person = fmap Person b

-- >>> person <*> addy
-- Just (Person (Name "Babe") (Address "old macdonald's"))
--

-- >>> Person <$> mkName "Babe" <*> addy
-- Just (Person (Name "Babe") (Address "old macdonald's"))
--
-- We still use fmap (via<$>) here for the first lifting over Maybe. After that, our (a -> b) is hiding in the f where f = Maybe, so we have to start using Applicative to keep mapping over it.


-- SIMPLIFIED
mkPerson' :: String -> String -> Maybe Person
mkPerson' n a = Person <$> mkName n <*> mkAddress a

------------------------------
-- -- p1066
-- Maybe Functor and the Name constructor
-- instance Functor Maybe where
--     fmap _ Nothing  = Nothing
--     fmap f (Just a) = Just (f a)
    
-- instance Applicative Maybe where
--     pure = Just 
--     Nothing <*> _ = Nothing
--     _ <*> Nothing = Nothing
--     Just f <*> Just a = Just (f a)

-- >>> mkName "babe"
-- Just (Name "babe")
--

-- >>> validateLength 25 "babe"
-- Just "babe"


-- Recall 
-- mkName :: String -> Maybe Name
-- mkName s = fmap Name $ validateLength 25 s 

--  mkName "babe" = fmap Name $ Just "babe"

-- >>> mkName "babe"
-- Just (Name "babe")



-- Notes p1069
-- (a -> b) -> f a -> f b
-- :t Name ::(String-> Name)
-- :t Just "babe" :: Maybe String
-- type M = Maybe 
--      (a -> b)   ->     f a   -> f b
-- (String-> Name) -> M String  -> M Name

-- Functor instance
-- fmap _ Nothing  = Nothing
-- fmap f (Just a) = Just (f a)

-- >>> fmap Name (Just "babe")
-- Just (Name "babe")
--
-- >>> mkName "babe"
-- Just (Name "babe")
--
-- Recall 
-- mkName s = fmap Name $ validateLength 25 s 
-- mkName "babe" = fmap Name $ Just "babe"

--------------------------------
-- p1070
-- Maybe Applicative and Person

