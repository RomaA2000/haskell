{-# LANGUAGE InstanceSigs #-}
{-
Module : Block1_3
Data type for 'NonEmpty'
-}
module Block1_3
  ( -- * The 'NonEmpty' type
    NonEmpty(..)
  ) where

data NonEmpty a = a :| [a]

instance Foldable NonEmpty where
  -- | Function 'foldMap' apply function to 'NonEmpty'
  foldMap :: Monoid m => (a -> m) -> NonEmpty a -> m
  foldMap func (h :| t) = mappend (func h) (foldMap func t)

instance Functor NonEmpty where
  -- | Function 'fmap' applies function to NonEmpty elements
  fmap :: (a -> b) -> NonEmpty a -> NonEmpty b
  fmap f (h :| t) = f h :| fmap f t

-- | Function 'inList' converts NonEmpty to List
inList :: NonEmpty a -> [a]
inList (a :| b) = (a : b)

instance Applicative NonEmpty where
  -- | Function 'pure' returns NonEmpty of one element
  pure :: a -> NonEmpty a
  pure x = x :| []

  -- | Function '<*>' applies NonEmpty to NonEmpty
  (<*>) :: NonEmpty (a -> b) -> NonEmpty a -> NonEmpty b
  (<*>) (func :| funcs) list@(x :| xs) = func x :| (fmap func xs ++ (funcs <*> (inList list)))

instance Traversable NonEmpty where
  -- | Function 'traverse' applies applicative f to NonEmpty
  traverse :: Applicative f => (a -> f b) -> NonEmpty a -> f (NonEmpty b)
  traverse f (a :| b) = (fmap (:|) (f a)) <*> (traverse f b)

instance Monad NonEmpty where
  -- | Function '>>=' applies function from a to NonEmpty b to NonEmpty of a
  (>>=) :: NonEmpty a -> (a -> NonEmpty b) -> NonEmpty b
  (>>=) (a :| t) f = b :| ((++) bs (t >>= inList . f))
    where b :| bs = f a
