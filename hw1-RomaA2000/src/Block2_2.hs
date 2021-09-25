{-# LANGUAGE InstanceSigs #-}
{-
Module : Block2_2
-}
module Block2_2
  ( -- * Functions
    splitOn
  , joinWith
  ) where

import           Data.List.NonEmpty (NonEmpty ((:|)), fromList, toList)

-- | Function 'splitOn' splits list with separator
splitOn :: Eq s => s -> [s] -> NonEmpty [s]
splitOn symbol = foldr splitOnImpl (fromList [[]])
  where splitOnImpl cur (head :| tailNotEmpty)
         | cur == symbol = [] :| (head : tailNotEmpty)
         | otherwise = (cur : head) :| tailNotEmpty

-- | Function 'joinWith' joins list with separator
joinWith :: s -> NonEmpty [s] -> [s]
joinWith symbol list = init $ foldr joinWithImpl [] (toList list)
  where joinWithImpl now chunk = now ++ symbol : chunk
