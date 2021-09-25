{-# LANGUAGE InstanceSigs #-}
{-
Module : Block1_1
Function for summing ints from string
-}
module Block1_1
  ( -- * The 'maybeSum' function
    maybeSum
  ) where

import           Text.Read (readMaybe)

-- | Function 'maybeSum' returns Maybe Int from string
maybeSum:: String -> Maybe Int
maybeSum str = fmap sum (traverse readMaybe (words str))
