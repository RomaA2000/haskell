{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-
Module : Block2_2
Function for calculating Simple Moving average from List
-}
module Block2_2
  ( -- * The 'moving' function
    moving
  ) where

import           Control.Monad.State.Lazy

-- | Function 'moving' calculates Simple Moving average from List
moving ::  forall a.  (Fractional a) => Int -> [a] -> [a]
moving num _ | num <= 0 = error "Invalid Argument"
moving num list = fst (runState (mapM movingImpl list) [])
  where
    movingImpl :: a -> State [a] a
    movingImpl = \val -> do
      now <- get
      let new = (val : (take (num - 1) now))
      let len = (fromIntegral $ length new)
      let newSum = (sum new)
      put $ new
      return $ newSum / len
