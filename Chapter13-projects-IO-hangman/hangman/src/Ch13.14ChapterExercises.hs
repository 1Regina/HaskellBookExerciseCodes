-- Modifying code

-- 1)
-- see VigenereCiphersIO.hs and CaesarCipherIO.hs `encodeInput` and `decodeInput` functions

-- 2)

import Control.Monad
import System.Exit (exitSuccess)

palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  case (line1 == reverse line1) of
    True -> putStrLn "It's a palindrome!"
    False -> do
      putStrLn "Nope!"
      exitSuccess  -- just need this to exit

-- 3)
-- not so sure
import Control.Monad
import Data.Char (toLower)
import System.Exit (exitSuccess)

palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  let line1' = filter (\x -> x `elem` ['a'..'z']) (map toLower line1) -- to break the strings into char the word
  case (line1' == reverse line1') of
    True -> putStrLn "It's a palindrome!"
    False -> do
      putStrLn "Nope!"
      exitSuccess

-- 4)

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid =
    NameEmpty
  | AgeTooLow
  | PersonInvalidUnknown String
  deriving (Eq, Show)

mkPerson :: Name
         -> Age
         -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 =
      Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise =
      Left $ PersonInvalidUnknown $
        "Name was: " ++ show name ++
        " Age was: " ++ show age


gimmePerson :: IO ()
gimmePerson = do
  putStr "Enter a name: "
  name <- getLine
  putStr "Enter an age: "
  ageString <- getLine
  putStrLn $ case mkPerson name (read ageString) of --(read age) :: Integer  -- convert from String to Int
    Left NameEmpty -> "An error occurred: name was empty"
    Left AgeTooLow -> "An error occurred: age was too low"
    Left (PersonInvalidUnknown e) -> "An error occurred with unknown type for person: " ++ e
    Right p -> "Yay! Successfully got a person: " ++ show p