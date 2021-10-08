-- Cannot test and run bcos cannot import System.Random (randomRIO)
module Main where
 -- Ch13.10 import moudules
import Control.Monad (forever)-- [1]
import Data.Char(toLower)-- [2]
import Data.Maybe(isJust)-- [3]
import Data.List(intersperse)-- [4]
import System.Exit(exitSuccess)-- [5]
import System.IO (BufferMode(NoBuffering),
                  hSetBuffering,
                  stdout)-- [6]
import System.Random (randomRIO)-- [7] --fail to load. Add into additions.cabal
-- import System.Random(randomIO) 
import System.IO
import Test.Hspec --added for Ch14.07 -- fail to load. Sorted after settling random

-- First, had to add `deriving (Eq)` to Puzzle:
data Puzzle =
  Puzzle String [Maybe Char] [Char]
  deriving (Eq)

-- To test, worked in existing ch13 project directory. Added
-- hspec to hangman.cabal build-depends, then added this to Main.hs
-- (replacing existing main function):

-- see below for fillinharacter, handleGuess etc
main :: IO ()
main = hspec $ do
  describe "fillInCharacter" $ do
    context "a puzzle where the guessed char is in the puzzle" $ do
      it "fills in the character in the puzzle and adds char to guesses" $ do
        (fillInCharacter (freshPuzzle "foo") 'o') `shouldBe`
          Puzzle "foo" [Nothing, Just 'o', Just 'o'] "o"
    context "a puzzle where the guessed char is not in the puzzle" $ do
      it "doesn't fill in the puzzle but adds char to guesses" $ do
        (fillInCharacter (freshPuzzle "foo") 'z') `shouldBe`
          Puzzle "foo" [Nothing, Nothing, Nothing] "z"
  describe "handleGuess" $ do
    context "a puzzle with an existing guess" $ do
      let puzzle = (Puzzle "foo" [Nothing, Nothing, Nothing] "z")
      context "re-guessing the same char" $ do
        it "doesn't change the puzzle" $ do
          puzzle' <- handleGuess puzzle 'z'
          puzzle' `shouldBe` puzzle
      context "guessing a new char where the char is in the puzzle" $ do
        it "updates the puzzle and guesses" $ do
          puzzle' <- handleGuess puzzle 'f'
          puzzle' `shouldBe` (Puzzle "foo" [Just 'f', Nothing, Nothing] "fz")
      context "guessing a new char where the char is not in the puzzle" $ do
        it "updates the guesses but not the puzzle" $ do
          puzzle' <- handleGuess puzzle 'x'
          puzzle' `shouldBe` (Puzzle "foo" [Nothing, Nothing, Nothing] "xz")
    context "a puzzle without existing guesses" $ do
      let puzzle = (Puzzle "foo" [Nothing, Nothing, Nothing] "")
      context "guessing a new char where the char is in the puzzle" $ do
        it "updates the puzzle and guesses" $ do
          puzzle' <- handleGuess puzzle 'f'
          puzzle' `shouldBe` (Puzzle "foo" [Just 'f', Nothing, Nothing] "f")
      context "guessing a new char where the char is not in the puzzle" $ do
        it "updates the guesses but not the puzzle" $ do
          puzzle' <- handleGuess puzzle 'x'
          puzzle' `shouldBe` (Puzzle "foo" [Nothing, Nothing, Nothing] "x")


--Ch13.12

-- arguments are
--   word we're trying to guess
--   characters we've filled in so far
--   letters guessed so far

-- data Puzzle = 
--   Puzzle String [Maybe Char] [Char]  deriving (Eq)


instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
    (intersperse ' ' $
     fmap renderPuzzleChar discovered)
    ++ " Guessed so far: " ++ guessed


-- first argument is aString, which will be the word that is in play. It will returna value of type Puzzle
-- 1. first value in the output will be the same string as the argument to the function.
-- 2. The second value will be the result of mapping a functionover thatStringargument. Consider usingc onstin the mapped function, as it will always return its first argument,no matter what its second argument is.
-- 3. For purposes of this function, the final argument ofPuzzleis an empty list

freshPuzzle :: String -> Puzzle
freshPuzzle x = Puzzle x (map (const Nothing) x) []

-- 1. two arguments, and one of those is of typePuzzle, which is a product of three types. But for the purpose of this function, we only care about the first argument to Puzzle
-- 2. can use underscores to signal that there are valueswe don’t care about and tell the function to ignore the
charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle word _ _) c = elem c word  -- word = word to guess


-- >>>  :t elem
-- elem :: (Foldable t, Eq a) => a -> t a -> Bool
--


-- this time we don’t care whether theCharis part of the String argument—this time, we want to check and see if it is an ele-ment of the guessed list.
alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed) c = elem c guessed  

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing  = '_'
renderPuzzleChar (Just x) = x
--  xs = [n, Just 'h', Nothing, Just 'e', n]
-- >>> fmap renderPuzzleChar xs

--

-- to insert a cor-rectly guessed character into the string. This if-then-else expression checks to see if the guessed character is one of the word characters.
-- If guesed word = a word character, wraps it in a Just, because our puzzle word is a list of Maybe values
fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word
                 filledInSoFar s) c =         -- s = alr guessed chars; c = guessing now
  Puzzle word newFilledInSoFar (c : s)        -- newFilledInSoFar == accum of filledInSoFar
  where zipper guessed wordChar guessChar =   --zipperis a combining function for deciding how to handle the character in the word
          if wordChar == guessed              -- guessed is the character the player guesses.
          then Just wordChar                  -- characters in the original word that they’re trying to guess.
          else guessChar                      -- guessCharis the character that the player has guessed
        newFilledInSoFar =                    -- newFilledInSoFaris the new state of the puzzle, which uses zipWith and the zipper combining function to fill in char-acters in the puzzle. The zipper function is first applied to the character the player just guessed, because that doesn’tchange. Then, it’s zipped across two lists. One list is word,which is the word the user is trying to guess. The second list, filledInSoFar, is the puzzle state we’re starting with of type[Maybe Char]. That’s telling us which characters in word have been guessed.
          zipWith (zipper c) 
            word filledInSoFar               -- make ournewFilledInSoFarby usingzipWith. You may remember this from Chapter 9, on lists. It’s going to zip the word with the filledInSoFar values, while applying the zipper function from just above it to those values as it does so.


-- The case expression is to give different responses based on whether the guessed character:
-- • Had already been guessed previously.
-- • Is in the word and needs to be filled in.
-- •Was not previously guessed but also isn’t in the puzzleword.
handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn "You already guessed that character, pick something else!"
      return puzzle
    (True, _) -> do
      putStrLn "This character was in the word, filling in the word accordingly."
      return (fillInCharacter puzzle guess)
    (False, _) -> do
      putStrLn "This character wasn't in the word, try again."
      return (fillInCharacter puzzle guess)


gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guessed) =
  if (length guessed) > 7 then
    do putStrLn "You lose!"
       putStrLn $
         "The word was: " ++ wordToGuess
       exitSuccess
  else return ()


-- recall that our puzzle word is a list of Maybe values, so when each character is represented by a Just Char rather than a Nothing, you win the game and we exit
gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _) =
  if all isJust filledInSoFar then
    do putStrLn "You win!"
       exitSuccess
  else return ()


-- use forever to execute this series of actions indefinitely:
runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameOver puzzle
  gameWin puzzle
  putStrLn $
    "Current puzzle is: " ++ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
   [c] -> handleGuess puzzle c >>= runGame
   _   ->
     putStrLn "Your guess must\
             \ be a single character"