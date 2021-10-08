module Vigenere where
import Control.Monad
import Data.Char
import System.Exit (exitSuccess)
import Test.QuickCheck

-- chrchr :: Int -> Char
-- ordord :: Char-> Int

encode :: Char -> Int
encode ch = ord ch - ord 'a'
-- >>> encode 'f'
-- 5


decode :: Int -> Char
decode n = chr (ord 'a' + n)
-- >>> decode (4)
-- 'e'
--

shift :: (Int -> Int -> Int) -> Int -> Char -> Char
shift f n ch = decode $ mod (f (encode ch) n) 26
-- >>> shift (+) (5) 'f'
-- 'k'
--

rightShift :: Int -> Char -> Char
rightShift n ch = decode $ mod ((+) (encode ch) n) 26
-- >>> rightShift (3) 'b'
-- 'e'
--

leftShift :: Int -> Char -> Char
leftShift n ch = decode$ mod ((-) (encode ch) n) 26
-- >>> leftShift (3) 'b'


-- 1. keep repeating the key word 'Ally' and concat that
-- 2. Encode the repeating keyword to get the sequence of integer for 'Ally' continual string
-- 3a. Right shift every character of the message according to the integer of the encoding keyword
-- 3b. Do this correspondingly one int for each char in the message with zipWith
vigenere :: String -> String -> String
vigenere keyword = zipWith (rightShift . encode) (concat $ repeat keyword)
-- >>> vigenere "ally" "meetatdawn"
-- "mppraeoywy"


unVigenere :: String -> String -> String
unVigenere keyword = zipWith (leftShift . encode) (concat $ repeat keyword)
-- >>> unVigenere "ally" "mppraeoywy"
-- "meetatdawn"

encodeInput :: IO ()
encodeInput = do
  putStrLn "Input plaintext to encode:"
  plaintext <- getLine
  putStrLn "Input codeword"
  codeword <- getLine
  putStrLn $ vigenere keyword plaintext

decodeInput :: IO ()
decodeInput = do
  putStrLn "Input ciphertext to decode:"
  ciphertext <- getLine
  putStrLn "Input codeword"
  codeword <- getLine
  putStrLn $ unVigenere keyword ciphertext 


vigenereIdentity :: String -> String -> Bool
vigenereIdentity keyword plaintext = (unVigenere keyword (vigenere keyword plaintext ) ) == plaintext

qc_vigenereIdentity :: IO ()
qc_vigenereIdentity = do
      quickCheck vigenereIdentity

-- cannot test properly due to done only in small letters
