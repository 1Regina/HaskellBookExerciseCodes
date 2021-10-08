module MyWords where
--Q1
myWords :: String -> [String]
myWords []       = []
myWords (' ':s)  = myWords s  -- required to avoid an infinite ,
myWords s        = w : myWords t
  where
    w = takeWhile (/= ' ') s -- takes every space with letter
    t = dropWhile (/= ' ') s -- gets rid of the spaces between words

-- >>>  myWords "sheryl has fun alot"
-- ["sheryl","has","fun","alot"]
--

myWords' :: String -> [String]
myWords' "" = []
myWords' x
  | dropWhile (/=' ') x == "" = [x] 
  | otherwise = (takeWhile (/=' ') x) : myWords'(dropWhile (==' ')(dropWhile (/=' ') x))
                                        -- takeWhile to take first word and put ' ' to remove whitespace delimiter
                                        -- dropWhile to take subsequent words 
                                        -- (dropWhile (/=' ') to take the sentence after the first word " sheryl has fun alot"
                                        -- trim whitespace delimiter at start of string with dropWhile (==' ')
                                        -- See https://stackoverflow.com/questions/53461230/how-would-i-split-a-string-after-the-spaces-in-haskell

-- >>>  myWords' "sheryl has fun alot too"
-- ["sheryl","has","fun","alot","too"]
--

-- >>>  myWords' "sheryl"
-- ["sheryl"]
--
--Incorrect
-- myWords1 :: String -> [String]
-- myWords1 "" = []
-- myWords1 x
--   | d x == "" = [x]
--   | otherwise = (t x) : myWords1 (d (d x)
--       where 
--         d = dropWhile (/=' ')
--         t = takeWhile (/=' ')


-- >>>  myWords1 "sheryl has fun alot"
-- (Error while loading modules for evaluation)
-- [1 of 1] Compiling MyWords          ( C:\Users\regina\Desktop\SMU\HaskellBook\Ch09.6ExtractListMyWords.hs, interpreted )
-- <BLANKLINE>
-- C:\Users\regina\Desktop\SMU\HaskellBook\Ch09.6ExtractListMyWords.hs:31:7-11: error:
--     parse error on input `where'
-- Failed, no modules loaded.
--


