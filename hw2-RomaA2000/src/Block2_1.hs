{-# LANGUAGE InstanceSigs #-}
{-
Module : Block2_1
Data type for 'Expr'
Data type for 'ExprNoExcept'
Data type for 'ArithmeticError'
Function for evaluating 'Expr'
-}
module Block2_1
  ( -- * The 'Expr' type
    Expr(..),
    -- * The 'ExprNoExcept' type
    ExprNoExcept(..),
    -- * The 'ArithmeticError' type
    ArithmeticError(..),
    -- * The 'eval' function
    eval
  ) where

import           Control.Applicative (liftA2)

data ExprNoExcept = ExprNoExcept (Int -> Int -> Int) Char Expr Expr

data Expr
  = Const Int
  | NoExcept ExprNoExcept
  | Divide Expr Expr
  | Power Expr Expr

data ArithmeticError
  = DivisionByZero
  | NegativeExponent
  deriving (Eq, Show)

-- | Function 'evalNoExcept' evaluates 'ExprNoExcept'
evalNoExcept :: ExprNoExcept -> Either ArithmeticError Int
evalNoExcept (ExprNoExcept op _ l r) = (liftA2 op) (eval l) (eval r)

-- | Function 'eval' evaluates 'Expr'
eval :: Expr -> Either ArithmeticError Int
eval (Const val) = return val
eval (NoExcept val) = evalNoExcept val
eval (Power l r) = eval r >>= \r_v -> if r_v < 0
                                  then Left NegativeExponent
                                  else eval l >>= \l_v -> return (l_v ^ r_v)
eval (Divide l r) = eval r >>= \r_v -> if r_v == 0
                                  then Left DivisionByZero
                                  else eval l >>= \l_v -> return (div l_v r_v)
