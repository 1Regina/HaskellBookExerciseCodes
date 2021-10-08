data Person1 = 
    MkPerson String Int 
    deriving (Eq,Show)

jm1 :: Person1
jm1 = MkPerson "julie1" 18
ca1 :: Person1
ca1 = MkPerson "chris1" 6 

namae:: Person1-> String
namae (MkPerson s _) = s


agee :: Person1-> Int
agee (MkPerson _ n) = n

-- >>> agee jm1
-- 18
-- >>> namae jm1
-- "julie1"
--


-- Record Syntax

data Person = 
    Person { name :: String, 
             age:: Int} 
             deriving(Eq,Show)


-- >>> Person "Papu" 5-- attempting to use module ‘fake_uid:Main’ (/home/regina/haskell/HaskellBook/Ch11.11ExercisesRecordSyntac.hs) which is not loaded
-- Person {name = "Papu", age = 5}
-- >>> papu = Person "Papu" 5
-- >>> age papu
-- 5

-- sample data
-- >>> jm = Person "julie" 108
-- >>> age jm
-- >>> name jm
-- 108
-- "julie"
--

-- >>> ca = Person "chris" 16
-- >>> age ca
-- >>> name ca
-- 16
-- "chris"
--


