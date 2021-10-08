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
import System.Random (randomRIO)-- [7]
-- import System.Random(randomIO) 
import System.IO

-- main :: IO ()
-- main = do
--   putStrLn "hello world"

-- >>> toLower 'D'
-- >>> isJust Nothing
-- >>>  isJust (Just 10)
-- 'd'
-- False
-- True

-- >>> all even [2, 4, 6]
-- >>> all even [2, 4, 7]
-- >>> xs = [Just 'd', Nothing, Just 'g']
-- >>> all isJust xs
-- >>> ys = [Just 'd', Just 'o', Just 'g']
-- >>> all isJust ys
-- True
-- False
-- False
-- True
--

-- Foldable t=>(a-> Bool)->t a-> Bool
-- >>>  :t all :: (a -> Bool) -> [a] -> Bool
-- all :: (a -> Bool) -> [a] -> Bool :: (a -> Bool) -> [a] -> Bool
--
-- maybeAll::(a-> Bool)-> Maybe a-> Bool
-- maybeAll = all
-- >>> :t maybeAll
-- maybeAll :: (a -> Bool) -> Maybe a -> Bool
--
-- eitherAll::(a-> Bool)-> Either b a-> Bool
-- eitherAll=all

-- badAll::(a-> Bool)->(b->a)-> Bool
-- badAll= all
-- cant work bcos doesnt have foldable instance

-- >>> intersperse 0 [1, 1, 1]
-- [1,0,1,0,1]
--
-- >>>  :t randomRIO -- can only run in REPL
-- randomRIO :: Random a => (a, a) -> IO a
-- >>> randomRIO (1, 100)
-- (Error while loading modules for evaluation)
-- [1 of 1] Compiling Main             ( /Users/regina/HaskellProj/hangman/src/Main.hs, interpreted )
-- <BLANKLINE>
-- /Users/regina/HaskellProj/hangman/src/Main.hs:12:1-30: error:
--     Could not find module ‘System.Random’
--     Use -v (or `:set -v` in ghci) to see a list of the files searched for.
-- Failed, no modules loaded.

-- Ch13.11 Generate a word list
type WordList=[String]

allWords:: IO WordList
allWords= do
    dict <- readFile"data/dict.txt"
    return (lines dict)
-- >>> lines "aardvark\naaron"
-- ["aardvark","aaron"]
-- >>> length $ lines "aardvark\naaron"
-- 2
-- >>>  length $ lines "aardvark\naaron\nwoot"
-- 3
-- >>> length $ lines "aardvark aaron"
-- 1
--
-- lines are similar to words 
-- >>> words "aardvark aaron"
-- >>> words "aardvark\naaron"

minWordLength:: Int
minWordLength =  5 
maxWordLength:: Int
maxWordLength=9

gameWords :: IO WordList
gameWords = do
  aw <- allWords
  return (filter gameLength aw)
  where gameLength w =
          let l = length (w :: String)
          in     l >= minWordLength
              && l <  maxWordLength

randomWord :: WordList -> IO String
randomWord wl = do
  randomIndex <- randomRIO (0, (length wl) - 1)
  return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

--Ch13.12

-- arguments are
--   word we're trying to guess
--   characters we've filled in so far
--   letters guessed so far
data Puzzle = 
  Puzzle String [Maybe Char] [Char]


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
-- (Error while loading modules for evaluation)
-- [1 of 1] Compiling Main             ( /Users/regina/HaskellProj/hangman/src/Main.hs, interpreted )
-- <BLANKLINE>
-- /Users/regina/HaskellProj/hangman/src/Main.hs:11:1-32: error:
--     Could not find module ‘System.Random’
--     Use -v (or `:set -v` in ghci) to see a list of the files searched for.
-- Failed, no modules loaded.
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

-- main brings everything together: it gets a wordfrom the word list we generated, generates a fresh puzzle, and then executes the runGame actions we saw above, until such time as you guess all the characters in the word correctly or have made seven guesses, whichever comes first
main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  word <- randomWord'
  let puzzle =
        freshPuzzle (fmap toLower word)
  runGame puzzle