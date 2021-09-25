{-# LANGUAGE InstanceSigs #-}
{-
Module : Block3_1
-}
module Block3_1
  ( -- * Functions
    maybeConcat
  , eitherConcat
  ) where
import           Data.List.NonEmpty (NonEmpty ((:|)), fromList, toList)

-- | Function 'maybeConcat' concats list of 'Maybe a' in list a
maybeConcat :: [Maybe [a]] -> [a]
maybeConcat (Nothing : listTail)   = maybeConcat listTail
maybeConcat (Just head : listTail) = head ++ maybeConcat listTail
maybeConcat []                     = []

-- | Function 'eitherConcat' concats monoid list of 'Either' in tuple
eitherConcat :: (Monoid m1, Monoid m2) => [Either m1 m2] -> (m1, m2)
eitherConcat (Right head : listTail) = (mempty, head) <> eitherConcat listTail
eitherConcat (Left head : listTail)  = (head, mempty) <> eitherConcat listTail
eitherConcat []                      = (mempty, mempty)
