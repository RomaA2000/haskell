{-# LANGUAGE InstanceSigs #-}
{-
Module : Block3_3
Parsers for Integer and Correct Bracket Sequence
-}
module Block3_3
  ( -- * The 'parseInteger' function
    parseInteger,
    -- * The 'parseIntegerEof' function
    parseIntegerEof,
    -- * The 'parseDigitChars' function
    parseDigitChars,
    -- * The 'CBS' Data Type
    CBS(..),
    -- * The 'cBSParser' function
    cBSParser
  ) where

import           Block3_1
import           Block3_2
import           Control.Applicative (Alternative (..))
import           Data.Char           (isDigit)

data CBS
  = Empty
  | Concat CBS CBS
  | Surround CBS
  deriving (Show, Eq)

-- | Function 'cBSParser' parses CBS
cBSParser :: Parser Char CBS
cBSParser = cbsp <* eof
  where
    cbsp :: Parser Char CBS
    cbsp = ((<$>) Concat ((<$>) Surround (element '(' *> cbsp <* element ')')) <*> cbsp) <|> (Empty <$ ok)

-- | Function 'parseDigitChars' parses digits from input
parseDigitChars :: Parser Char [Char]
parseDigitChars = ((fmap (:) (element '-')) <|>  (fmap (flip const) (element '+')) <|> (fmap (flip const) ok)) <*> (some (satisfy isDigit))

-- | Function 'parseInteger' parses Integer from input
parseInteger :: Parser Char Integer
parseInteger = fmap read parseDigitChars

-- | Function 'parseInteger' parses Integer and eof from input
parseIntegerEof :: Parser Char Integer
parseIntegerEof = parseInteger <* eof
