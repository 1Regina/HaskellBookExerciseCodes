module Caesar where
import Data.Char
import Data.List
import Test.QuickCheck
-- chrchr :: Int -> Char
-- ordord :: Char-> Int

index :: Char -> Int
index ch = ord ch - ord 'a'
-- >>> index 'f'
-- 5
--
letter :: Int -> Char
letter n = chr (ord 'a' + n)
-- >>> letter (4)
-- 'e'
--

shift :: (Int -> Int -> Int) -> Int -> Char -> Char
shift f n ch = letter $ mod (f (index ch) n) 26
-- >>> shift (+) (5) 'f'
-- 'k'
--

rightShift :: Int -> Char -> Char
rightShift n ch = letter $ mod ((+) (index ch) n) 26
-- >>> rightShift (3) 'b'
-- 'e'
--

leftShift :: Int -> Char -> Char
leftShift n ch = letter $ mod ((-) (index ch) n) 26
-- >>> leftShift (3) 'b'
-- 'y'
--

--More simply but fail bcos char include {  , } etc which we dont want
shift1 :: (Int -> Int -> Int) -> Int -> Char -> Char
shift1 f n ch = letter  (f (index ch) n)
-- >>> shift1 (+) (3) 'z'
-- '}'

rightShift1 :: Int -> Char -> Char
rightShift1 n ch = letter ( (+) (index ch) n)

-- >>> rightShift1 (4) 'y'
-- '}'
--


leftShift1 :: Int -> Char -> Char
leftShift1 n ch = letter ( (-) (index ch) n)
-- >>> leftShift (4) 'a'
-- ']'


rightMove :: Int -> Char -> Char
rightMove = shift (+)
-- >>> rightMove (3) 'b'
-- 'e'
--

leftMove :: Int -> Char -> Char
leftMove = shift (-)
-- >>> leftMove 4 'b'
-- 'x'
--
caesar :: Int -> String -> String
caesar n = map (rightMove n)
-- >>> caesar 3 "asd"
-- "dvg"
--
unCaesar :: Int -> String -> String
unCaesar n = map (leftMove n)
-- >>> unCaesar 3 "asd"
-- "xpa"

encodeInput :: IO ()
encodeInput = do
  putStrLn "Input message to encode:"
  plaintext <- getLine
  putStrLn "Input shift"
  n <- getLine
  let n' = (read n) :: Int  -- need to conver from IO Int to Int with read
  putStrLn $ caesar n' plaintext

decodeInput :: IO ()
decodeInput = do
  putStrLn "Input ciphertext to decode:"
  ciphertext <- getLine
  putStrLn "Input shift"
  n <- getLine
  let shift' = (read n) :: Int  -- need to conver from IO Int to Int with read
  putStrLn $ unCaesar shift' ciphertext 


caesarIdentity :: Int -> String -> Bool
caesarIdentity i plaintext = (unCaesar i (caesar i plaintext ) ) == plaintext

qc_caesarIdentity :: IO ()
qc_caesarIdentity = do
      quickCheck caesarIdentity

--failed bcos didnt account for capital letters
-- *** Failed! Falsified (after 2 tests and 2 shrinks):    
-- 0
-- "A"