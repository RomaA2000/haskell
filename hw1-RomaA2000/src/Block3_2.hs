{-# LANGUAGE InstanceSigs #-}
{-
Module : Block3_2
-}
module Block3_2
  ( -- * The 'NonEmpty' type
    -- * The 'ThisOrThat' type
    -- * The 'Name' type
    -- * The 'Endo' type
    NonEmpty(..),
    ThisOrThat(..),
    Name(..),
    Endo(..)
  ) where

data NonEmpty valueType
  = valueType :| [valueType]
  deriving (Eq, Show)

instance Semigroup (NonEmpty valueType) where
  -- | Function '<>' associatively combines 'NonEmpty'
  (<>) :: NonEmpty a -> NonEmpty a -> NonEmpty a
  (head1 :| tail1) <> (head2 :| tail2) = head1 :| (tail1 ++ (head2 : tail2))

data ThisOrThat a b
  = This a
  | That b
  | Both a b
  deriving (Eq, Show)

instance Semigroup (ThisOrThat a b) where
  -- | Function '<>' associatively combines 'ThisOrThat'
  (<>) :: ThisOrThat a b -> ThisOrThat a b -> ThisOrThat a b
  This a <> This _ = This a
  This a <> That b = Both a b
  This a <> Both _ c = Both a c

  That a <> That _ = That a
  That a <> This b = Both b a
  That a <> Both c _ = Both c a

  Both a b <> This _ = Both a b
  Both a b <> That _ = Both a b
  Both a b <> Both _ _ = Both a b

newtype Name
  = Name String
  deriving (Eq, Show)

instance Semigroup Name where
  -- | Function '<>' associatively combines 'Name'
  (<>) :: Name -> Name -> Name
  (Name []) <> (Name n2) = Name n2
  (Name n1) <> (Name []) = Name n1
  (Name n1) <> (Name n2) = Name (n1 ++ "." ++ n2)

instance Monoid Name where
  -- | Function 'mempty' returns id element of 'Name'
  mempty  = Name []
  -- | Function 'mappend' associatively combines 'Name'
  mappend :: Name -> Name -> Name
  mappend = (<>)

newtype Endo a = Endo { getEndo :: a -> a }

instance Semigroup (Endo a) where
  -- | Function '<>' associatively combines 'Endo'
  (<>) :: Endo a -> Endo a -> Endo a
  (<>) e1 e2 = Endo $ (getEndo e1) . (getEndo e2)

instance Monoid (Endo a) where
  -- | Function 'mempty' returns id element of 'Endo'
  mempty  = Endo id
  -- | Function 'mappend' associatively combines 'Endo'
  mappend :: Endo a -> Endo a -> Endo a
  mappend = (<>)
