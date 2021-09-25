{-# LANGUAGE InstanceSigs #-}
{-
Module : Block2_1
Data type for Tree
-}
module Block1_3and2_1
  ( -- * The 'Tree' type
    Tree(..)
    -- * Functions for 'Tree'
  , isEmpty
  , size
  , find
  , insert
  , remove
  , fromList
  , toList
  ) where

import           Data.Foldable      hiding (find)
import           Data.List.NonEmpty (NonEmpty ((:|)))

data Tree dataType
  = Leaf
  | Node (NonEmpty dataType) (Tree dataType)  (Tree dataType)
  deriving (Show, Eq)

-- | Function 'isEmpty' checks if 'Tree' is empty
isEmpty :: Tree dataType -> Bool
isEmpty Leaf = True
isEmpty _    = False

-- | Function 'size' checks size of 'Tree'
size :: Tree dataType -> Int
size Leaf                = 0
size (Node dataList l r) = length dataList + size l + size r

-- | Function 'find' finds element in 'Tree'
find :: Ord dataType => Tree dataType -> dataType -> Bool
find (Node (dataInside :| _) l r) dataToFind =
  case compare dataToFind dataInside of
    LT -> find l dataToFind
    GT -> find r dataToFind
    EQ -> True
find Leaf _ = False

-- | Function 'insert' inserts element in 'Tree'
insert :: Ord dataType => Tree dataType -> dataType -> Tree dataType
insert (Node (dataInside :| dataCopies) l r) dataToInsert =
  case compare dataToInsert dataInside of
      LT -> (Node (dataInside :| dataCopies) (insert l dataToInsert) r)
      GT -> (Node (dataInside :| dataCopies) l (insert r dataToInsert))
      EQ -> (Node (dataToInsert :| (dataInside : dataCopies)) l r)
insert Leaf dataToInsert = (Node (dataToInsert :| []) Leaf Leaf)

-- | Function 'remove' removes element from 'Tree'
remove :: Ord dataType => Tree dataType -> dataType -> Tree dataType
remove (Node (dataListHead :| dataListTail) l r) dataToRemove
  | dataListHead == dataToRemove = case dataListTail of
                                     (d2 : newTail) -> (Node (d2 :| newTail) l r)
                                     _ -> mergeImpl l r
  | dataToRemove < dataListHead = (Node (dataListHead :| dataListTail) (remove l dataToRemove) r)
  | otherwise   = (Node (dataListHead :| dataListTail) l (remove r dataToRemove))
    where mergeImpl (Node vals left right) tree = (Node vals left (mergeImpl right tree))
          mergeImpl Leaf tree = tree
remove Leaf _ = Leaf

instance Foldable Tree where
  -- | Function 'foldr' folds 'Tree' with function
  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr _ zero Leaf           = zero
  foldr f zero (Node val l r) = foldr f (foldr f (foldr f zero r) val) l

  -- | Function 'foldMap' apply function to 'Tree'
  foldMap :: Monoid m => (a -> m) -> Tree a -> m
  foldMap f = foldr (mappend . f) mempty

-- | Function 'fromList' converts [] to 'Tree'
fromList :: Ord dataType => [dataType] -> Tree dataType
fromList = foldl insert Leaf
