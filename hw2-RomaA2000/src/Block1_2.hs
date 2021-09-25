{-# LANGUAGE InstanceSigs #-}
{-
Module : Block1_2
Data type for 'Tree'
-}
module Block1_2
  ( -- * The 'Tree' type
    Tree(..)
  ) where

data Tree a
  = Branch (Tree a) (Tree a)
  | Leaf a

instance Foldable Tree where
  -- | Function 'foldMap' apply function to 'Tree'
  foldMap :: Monoid m => (a -> m) -> Tree a -> m
  foldMap func (Leaf val)   = func val
  foldMap func (Branch l r) = mappend (foldMap func l) (foldMap func r)

instance Functor Tree where
  -- | Function 'fmap' applies function to Tree elements
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap f (Leaf a)     = Leaf (f a)
  fmap f (Branch l r) = Branch (fmap f l) (fmap f r)

instance Applicative Tree where
  -- | Function 'pure' returns Tree of one element
  pure :: a -> Tree a
  pure x = Leaf x

  -- | Function '<*>' applies Tree to Tree
  (<*>) :: Tree (a -> b) -> Tree a -> Tree b
  (<*>) (Leaf f) (Leaf x)                 = Leaf (f x)
  (<*>) func@(Leaf _) (Branch l r)        = Branch (func <*> l) (func <*> r)
  (<*>) (Branch l r) val@(Leaf _)         = Branch (l <*> val) (r <*> val)
  (<*>) (Branch f_l f_r) (Branch v_l v_r) = Branch (f_l <*> v_l) (f_r <*> v_r)

instance Traversable Tree where
  -- | Function 'traverse' applies applicative f to Tree
  traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
  traverse f (Leaf v)     = fmap Leaf (f v)
  traverse f (Branch l r) = (fmap Branch (traverse f l)) <*> (traverse f r)
