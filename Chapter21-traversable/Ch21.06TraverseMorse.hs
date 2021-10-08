module Ch2106TraverseMorse
       ( Morse
       , charToMorse
       , morseToChar
       , stringToMorse
       , letterToMorse
       , morseToLetter
       ) where

import qualified Data.Map as M
import Data.Maybe -- so as to use fromMaybe which was put in case string char arent translatable into Morse and vice versa

type Morse = String

letterToMorse :: (M.Map Char Morse) --M.from List—the M prefix tells us this comes from Data.Map
letterToMorse = M.fromList [
      ('a', ".-")
    , ('b', "-...")
    , ('c', "-.-.")
    , ('d', "-..")
    , ('e', ".")
    , ('f', "..-.")
    , ('g', "--.")
    , ('h', "....")
    , ('i', "..")
    , ('j', ".---")
    , ('k', "-.-")
    , ('l', ".-..")
    , ('m', "--")
    , ('n', "-.")
    , ('o', "---")
    , ('p', ".--.")
    , ('q', "--.-")
    , ('r', ".-.")
    , ('s', "...")
    , ('t', "-")
    , ('u', "..-")
    , ('v', "...-")
    , ('w', ".--")
    , ('x', "-..-")
    , ('y', "-.--")
    , ('z', "--..")
    , ('1', ".----")
    , ('2', "..---")
    , ('3', "...--")
    , ('4', "....-")
    , ('5', ".....")
    , ('6', "-....")
    , ('7', "--...")
    , ('8', "---..")
    , ('9', "----.")
    , ('0', "-----")
    ]

morseToLetter :: M.Map Morse Char
morseToLetter =
  M.foldrWithKey (flip M.insert) M.empty
                letterToMorse

charToMorse :: Char -> Maybe Morse
charToMorse c =
  M.lookup c letterToMorse

-- stringToMorse :: String -> Maybe [Morse]
-- stringToMorse s =
--   sequence $ fmap charToMorse s
-- ALTERNATIVELY, what we want to do
stringToMorse :: String -> Maybe [Morse]
stringToMorse = traverse charToMorse


morseToChar :: Morse -> Maybe Char
morseToChar m =
  M.lookup m morseToLetter

-- p1262
-- Ok, one module loaded.
-- *Ch2106TraverseMorse> morseToChar "-.-."
-- Just 'c'
-- *Ch2106TraverseMorse> morseToChar "gobbledegook"
-- Nothing
-- *Ch2106TraverseMorse> fromMaybe ' ' (morseToChar "-.-.")
-- 'c'
-- *Ch2106TraverseMorse>  stringToMorse "chris"
-- Just ["-.-.","....",".-.","..","..."]
-- *Ch2106TraverseMorse>  fromMaybe [] (stringToMorse "chris")
-- ["-.-.","....",".-.","..","..."]

-- p1262 - p1263
-- *Ch2106TraverseMorse> morse s = fromMaybe [] (stringToMorse s)
-- *Ch2106TraverseMorse> :t morse
-- morse :: String -> [Morse]
-- *Ch2106TraverseMorse> fmap morseToChar (morse "chris")
-- [Just 'c',Just 'h',Just 'r',Just 'i',Just 's']

-- sequence :: (Monad m,Traversable t) => t (m a) -> m (t a)
-- *Ch2106TraverseMorse> :t fmap
-- fmap :: Functor f => (a -> b) -> f a -> f b
-- *Ch2106TraverseMorse>  :t (sequence .) . fmap
-- (sequence .) . fmap
--   :: (Traversable t, Monad m) => (a1 -> m a2) -> t a1 -> m (t a2)
-- *Ch2106TraverseMorse> sequence $ fmap morseToChar (morse "chris")
-- Just "chris"
-- one more example
-- *Ch2106TraverseMorse>  fmap morseToChar (morse "julie")
-- [Just 'j',Just 'u',Just 'l',Just 'i',Just 'e']
-- *Ch2106TraverseMorse>  sequence $ fmap morseToChar (morse "julie")
-- Just "julie"
-- p1264
-- AIM
-- (sequence . ) . fmap = \f xs -> sequence (fmap f xs)
-- Traverse to resuce ... 
-- traverse = (sequence . ) . fmap = \f xs -> sequence (fmap f xs)
-- *Ch2106TraverseMorse>  traverse morseToChar (morse "chris")
-- Just "chris"
-- *Ch2106TraverseMorse>  traverse morseToChar (morse "julie")
-- Just "julie"