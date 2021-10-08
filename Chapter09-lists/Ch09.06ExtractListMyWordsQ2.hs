--Q2
module PoemLines where 
firstSen    = "Tyger Tyger, burning bright\n"
secondSen   = "In the forests of the night\n"
thirdSen    = "What immortal hand or eye\n"
fourthSen   = "Could frame thy fearful symmetry?"

sentences   = firstSen  ++  secondSen ++  thirdSen  ++  fourthSen

myLines :: String -> [String]
myLines "" = []
myLines x
  | dropWhile (/='\n') x == "" = [x]
  | otherwise = (takeWhile (/='\n') x) : myLines(dropWhile (=='\n')(dropWhile (/='\n') x))

  -- -- See https://stackoverflow.com/questions/53461230/how-would-i-split-a-string-after-the-spaces-in-haskell

-- >>> myLines sentences
-- ["Tyger Tyger, burning bright","In the forests of the night","What immortal hand or eye","Could frame thy fearful symmetry?"]
--
--
shouldEqual = [ "Tyger Tyger, burning bright", "In the forests of the night", "What immortal hand or eye", "Could frame thy fearful symmetry?"]

-- >>> myLines sentences == shouldEqual
-- True
--
main :: IO ()
main = print $ "Are they equal? " ++ show (myLines sentences == shouldEqual)


--alternative
-- mapM_ found at: https://stackoverflow.com/questions/5289779/printing-elements-of-a-list-on-new-lines
demo2 :: IO ()
demo2 = do
  putStrLn "Exercise 2:"
  putStrLn sentences
  mapM_ putStrLn (myLines sentences)

-- >>> demo2
-- Exercise 2:
-- Tyger Tyger, burning bright
-- In the forests of the night
-- What immortal hand or eye
-- Could frame thy fearful symmetry?
-- Tyger Tyger, burning bright
-- In the forests of the night
-- What immortal hand or eye
-- Could frame thy fearful symmetry?
--
