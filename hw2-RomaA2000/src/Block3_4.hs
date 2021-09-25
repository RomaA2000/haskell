{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-
Module : Block3_4
Parsers for IntegerLists
-}
module Block3_4
  ( -- * The 'parseListsIntEof' function
    parseListsIntEof,
    -- * The 'parseListsInt' function
    parseListsInt
  ) where

import           Block3_1
import           Block3_2
import           Block3_3
import           Control.Applicative (Alternative (..))
import           Control.Monad
import           Data.Char           (isSpace)
import           Text.Read

-- | Function 'skipSpaces' skips spaces
skipSpaces :: Parser Char ()
skipSpaces =  fmap (const ()) (many (satisfy isSpace))

-- | Function 'skipSpacesAndDelimiter' skips spaces and ','
skipSpacesAndDelimiter :: Parser Char Char
skipSpacesAndDelimiter = skipSpaces *> element ','

-- | Function 'parseListsInt' parses Lists of Ints
parseListsInt :: Parser Char [[Int]]
parseListsInt = ((fmap (:) parseList) <*> (many (skipSpacesAndDelimiter *> parseList))) <|> (fmap (const []) ok)
  where
    unpack :: [Char] -> Parser Char Int
    unpack s = case readMaybe s of
                      Just val -> return val
                      Nothing  -> empty

    parseInt :: Parser Char Int
    parseInt = (skipSpaces *> parseDigitChars) >>= unpack

    parseList :: Parser Char [Int]
    parseList = parseInt >>= (flip replicateM (skipSpacesAndDelimiter *> skipSpaces *> parseInt))

-- | Function 'parseListsInt' parses Lists of Ints and eof
parseListsIntEof :: Parser Char [[Int]]
parseListsIntEof = parseListsInt <* skipSpaces <* eof

