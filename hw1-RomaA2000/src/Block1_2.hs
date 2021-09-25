{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE InstanceSigs   #-}
{-
Module : Block1_2
Data type for Num
-}
module Block1_2
  ( -- * The 'Nat' type
    Nat(..)
    -- * Functions for 'Nat'
  , natFromInt
  , intFromNat
  , isEven
  , divNat
  , modNat
  ) where

data Nat
  = Z
  | S Nat
  deriving (Show)

instance Eq Nat where
  -- | Function '==' checks equality of 'Nat's
  (==) :: Nat -> Nat -> Bool
  Z      == Z      = True
  (S _)  == Z      = False
  Z      == (S _)  = False
  (S n1) == (S n2) = n1 == n2

instance Ord Nat where
  -- | Function '<=' checks ord of 'Nat's
  (<=) :: Nat -> Nat -> Bool
  Z      <= _      = True
  (S _)  <= Z      = False
  (S n1) <= (S n2) = n1 <= n2

instance Num Nat where
  -- | Function '+' adds two 'Nat's
  (+) :: Nat -> Nat -> Nat
  n  + Z      = n
  n1 + (S n2) = S $ n1 + n2

  -- | Function '-' subs two 'Nat's
  (-) :: Nat -> Nat -> Nat
  Z      - _      = Z
  n      - Z      = n
  (S n1) - (S n2) = n1 - n2

  -- | Function '*' mul two 'Nat's
  (*) :: Nat -> Nat -> Nat
  _  * Z      = Z
  n1 * (S n2) = (n1 * n2) + n1

  -- | Function 'signum' get signum os 'Nat'
  signum :: Nat -> Nat
  signum Z     = Z
  signum (S _) = S Z

  -- | Function 'fromInteger' get 'Nat' from 'Integer'
  fromInteger :: Integer -> Nat
  fromInteger = natFromInt

  -- | Function 'abs' get positive 'Nat' from 'Nat'
  abs :: Nat -> Nat
  abs = id

-- | Function 'natFromInt' get 'Nat' from 'Int'
natFromInt :: Num a => Ord a => a -> Nat
natFromInt a =
  if a > 0
  then S $ natFromInt $ a - 1
  else if a == 0
  then Z
  else error "Only positive Nums are convertable"

-- | Function 'intFromNat' get 'Integer' from 'Nat'
intFromNat :: Nat -> Integer
intFromNat Z     = 0
intFromNat (S n) = 1 + (intFromNat n)

-- | Function 'isEven' checks if 'Nat' is even
isEven :: Nat -> Bool
isEven Z     = True
isEven (S n) = not $ isEven n

-- | Function 'modDivImpl' divs two 'Nat's
modDivImpl :: Nat -> Nat -> Nat -> (Nat, Nat)
modDivImpl n1 n2 d = if n1 < n2
                     then (n1, d)
                     else if n2 == Z
                     then error "Div by Zero"
                     else modDivImpl (n1 - n2) n2 (S d)

-- | Function 'divNat' divs two 'Nat's
divNat :: Nat -> Nat -> Nat
divNat n1 n2 = snd $ modDivImpl n1 n2 Z

-- | Function 'modNat' mods two 'Nat's
modNat :: Nat -> Nat -> Nat
modNat n1 n2 = fst $ modDivImpl n1 n2 Z
