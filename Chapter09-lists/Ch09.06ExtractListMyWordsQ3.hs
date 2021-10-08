--Q3

mySplit :: Char -> String -> [String]
mySplit _ "" = []
mySplit c str
  | dropWhile (/=c) str == "" = [str]
  | otherwise = (takeWhile (/=c) str) : (mySplit c (dropWhile (==c) (dropWhile (/=c) str)))
-- See https://stackoverflow.com/questions/53461230/how-would-i-split-a-string-after-the-spaces-in-haskell

myWords' :: String -> [String]
myWords' x = mySplit ' ' x

myLines' :: String -> [String]
myLines' x = mySplit '\n' x

firstSen    = "Tyger Tyger, burning bright\n"
secondSen   = "In the forests of the night\n"
thirdSen    = "What immortal hand or eye\n"
fourthSen   = "Could frame thy fearful symmetry?"

sentences   = firstSen  ++  secondSen ++  thirdSen  ++  fourthSen

-- >>> myLines' sentences
-- ["Tyger Tyger, burning bright","In the forests of the night","What immortal hand or eye","Could frame thy fearful symmetry?"]
--
