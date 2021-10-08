{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative             ((<|>))
import           Data.ByteString                 (ByteString)
import           Text.Parser.Char                (CharParsing, char)
import           Text.Parser.Combinators         ((<?>), try)
import qualified Data.Attoparsec.ByteString as A (Parser, parseOnly)
import qualified Text.Parsec                as P (Parsec, parseTest)
import qualified Text.Trifecta              as T (Parser, parseString)

-- Helper function to run a trifecta parser and print the result:
trifP :: Show a => T.Parser a -> String -> IO ()
trifP p = print . T.parseString p mempty

-- Helper function to run a parsec parser and print the result:
parsecP :: (Show a) => P.Parsec String () a -> String -> IO ()
parsecP = P.parseTest

-- Helper function to run attoparsec parser and print the result:
attoP :: Show a => A.Parser a -> ByteString -> IO ()
attoP p = print . A.parseOnly p

---
-- Here’s our first parser. It attempts to parse '1' followed by '2' or '3'. This parser does not backtrack:
nobackParse :: (Monad f, CharParsing f) => f Char
nobackParse = (char '1' >> char '2') <|> char '3'

-- This parser has similar behavior to the previous one, except it backtracks if the first parse fails. Backtracking means that the input cursor returns to where it was before the failed parser consumed input:
tryParse :: (Monad f, CharParsing f) => f Char
tryParse = try (char '1' >> char '2') <|> char '3'


-- using the <?> operator to annotate parse rules any time you use try. So that the error will list the parses it attempts before it fails
tryAnnot :: (Monad f, CharParsing f) => f Char
tryAnnot = (try (char '1' >> char '2') <?> "Tried 12") <|>
           (char '3' <?> "Tried 3")

---

main :: IO ()
main = do
  putStrLn "\nParsing with Trifecta:"
  trifP nobackParse "13"
  trifP tryParse "13"
  trifP nobackParse "12"
  trifP nobackParse "3"
  trifP tryAnnot "13"

  putStrLn "\nParsing with Parsec:"
  parsecP nobackParse "13"
  parsecP tryParse "13"
  parsecP nobackParse "12"
  parsecP nobackParse "3"
  parsecP tryAnnot "13"

  putStrLn "\nParsing Attoparsec:"
  attoP nobackParse "13"
  attoP tryParse "13"
  attoP nobackParse "12"
  attoP nobackParse "3"
  attoP tryAnnot "13"
