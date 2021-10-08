module Phone where

import Data.Char (isUpper, toLower)
import Data.List (elemIndex, find, sort, group, maximumBy)
import Data.Ord


type Digit = Char
type Presses = Int -- 1 and up

data Button = Button Digit String

-- 1
data Phone = Phone [Button]

validDigits :: String
validDigits = ['0'..'9'] ++ "*#"

validLetters :: String
validLetters = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " ."

isValidDigit :: Char -> Bool
isValidDigit = (flip elem) validDigits

isValidLetter :: Char -> Bool
isValidLetter = (flip elem) validLetters

phone :: Phone
phone = Phone
  [ Button '1' "",     Button '2' "abc", Button '3' "def"
  , Button '4' "ghi",  Button '5' "jkl", Button '6' "mno"
  , Button '7' "pqrs", Button '8' "tuv", Button '9' "wxyz"
  , Button '*' "",     Button '0' " ",   Button '#' "."
  ]

tapsOnButton :: Button -> Char -> [(Digit, Presses)]
tapsOnButton (Button d s) c
  | not (isValidLetter c) = []
   -- if c is a number, then + 1 to length
  | c == d                = [(d, length s + 1)]
  -- if c is upper, hit the '*' and the button n + 1 where n is the elemIndex as index starts at 0
  | isUpper c             = case elemIndex (toLower c) s of
                              Nothing -> []
                              Just n  -> [('*', 1), (d, n + 1)]
  | otherwise             = case elemIndex c s of
                              Nothing -> []
                              Just n  -> [(d, n + 1)]
-- >>> tapsOnButton (Button '7' "pqrs") 'r'
-- [('7',3)]
--

-- 2 Same as tapsOnButoon but using the entire table
reverseTaps :: Phone -> Char -> [(Digit, Presses)]
reverseTaps (Phone buttons) c =
  maybe [] id $ find nonEmpty $ map (flip tapsOnButton c) buttons
  where
    nonEmpty [] = False
    nonEmpty _  = True

-- >>> reverseTaps phone 'r'
-- [('7',3)]
--

convo :: [String]
convo =
  [ "Wanna play 20 questions"
  , "Ya"
  , "U 1st haha"
  , "Lol ok. Have u ever tasted alcohol lol"
  , "Lol ya"
  , "Wow ur cool haha. Ur turn"
  , "Ok. Do u think I am pretty Lol"
  , "Lol ya"
  , "Haha thanks just making sure rofl ur turn"
  ]

-- Given a string, get the presses for each char, strung tog into a list
cellPhonesDead :: Phone -> String -> [(Digit, Presses)]
cellPhonesDead phone = concatMap (reverseTaps phone)

-- >>> cellPhonesDead phone "Lol ya"
-- [('*',1),('5',3),('6',3),('5',3),('0',1),('9',3),('2',1)]
--


-- The following will convert convo into the keypresses
-- required for each message
keypresses :: [[(Digit, Presses)]]
keypresses = map (cellPhonesDead phone) convo

-- >>> keypresses 
-- [[('*',1),('9',1),('2',1),('6',2),('6',2),('2',1),('0',1),('7',1),('5',3),('2',1),('9',3),('0',1),('2',4),('0',2),('0',1),('7',2),('8',2),('3',2),('7',4),('8',1),('4',3),('6',3),('6',2),('7',4)],[('*',1),('9',3),('2',1)],[('*',1),('8',2),('0',1),('1',1),('7',4),('8',1),('0',1),('4',2),('2',1),('4',2),('2',1)],[('*',1),('5',3),('6',3),('5',3),('0',1),('6',3),('5',2),('#',1),('0',1),('*',1),('4',2),('2',1),('8',3),('3',2),('0',1),('8',2),('0',1),('3',2),('8',3),('3',2),('7',3),('0',1),('8',1),('2',1),('7',4),('8',1),('3',2),('3',1),('0',1),('2',1),('5',3),('2',3),('6',3),('4',2),('6',3),('5',3),('0',1),('5',3),('6',3),('5',3)],[('*',1),('5',3),('6',3),('5',3),('0',1),('9',3),('2',1)],[('*',1),('9',1),('6',3),('9',1),('0',1),('8',2),('7',3),('0',1),('2',3),('6',3),('6',3),('5',3),('0',1),('4',2),('2',1),('4',2),('2',1),('#',1),('0',1),('*',1),('8',2),('7',3),('0',1),('8',1),('8',2),('7',3),('6',2)],[('*',1),('6',3),('5',2),('#',1),('0',1),('*',1),('3',1),('6',3),('0',1),('8',2),('0',1),('8',1),('4',2),('4',3),('6',2),('5',2),('0',1),('*',1),('4',3),('0',1),('2',1),('6',1),('0',1),('7',1),('7',3),('3',2),('8',1),('8',1),('9',3),('0',1),('*',1),('5',3),('6',3),('5',3)],[('*',1),('5',3),('6',3),('5',3),('0',1),('9',3),('2',1)],[('*',1),('4',2),('2',1),('4',2),('2',1),('0',1),('8',1),('4',2),('2',1),('6',2),('5',2),('7',4),('0',1),('5',1),('8',2),('7',4),('8',1),('0',1),('6',1),('2',1),('5',2),('4',3),('6',2),('4',1),('0',1),('7',4),('8',2),('7',3),('3',2),('0',1),('7',3),('6',3),('3',3),('5',3),('0',1),('8',2),('7',3),('0',1),('8',1),('8',2),('7',3),('6',2)]]
--



-- 3 no. of taps for each message
fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps x = sum (map snd x)

-- 4. sort all letter in word, group them, take the char with the max count
mostPopularLetter :: String -> Char
mostPopularLetter x = fst (maximumBy (comparing snd) letterCounts) where
  letterCounts = map (\x' -> (head x', length x')) $ group $ sort x

-- >>> mostPopularLetter "Hello"
-- 'l'

--need help--
mostPopularLetterCosts1 :: [String] -> [(Char, Presses)]
mostPopularLetterCosts1 convo = map (\x -> (x, (fingerTaps $ (reverseTaps phone x)))) (map mostPopularLetter convo)
-- >>> mostPopularLetterCosts1 convo
-- [('n',2),('a',1),('h',2),(' ',1),('y',3),(' ',1),(' ',1),('y',3),(' ',1)]


-- mostPopularLetterCosts :: String -> [(Char, Presses)]
-- mostPopularLetterCosts y = map (\x -> (x, (fingerTaps $ (reverseTaps phone x)))) (map mostPopularLetter y)
-- -- >>> mostPopularLetterCosts "hello"



coolestLtr :: [String] -> Char
coolestLtr = mostPopularLetter . concat
-- >>> coolestLtr ["Hello World", "Everyone"]
-- 'o'


coolestWord :: [String] -> String
coolestWord x = fst (maximumBy (comparing snd) wordCounts) where
  allWords = concat (map words x)
  wordCounts = map (\x' -> (head x', length x')) $ group $ sort allWords
-- >>> coolestWord ["Hello World", "Everyone", "Hello"]
-- "Hello"
