{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-
Module : Task1Norm
Functions for lazy operations with points
-}
module Task1Norm
  (
    -- | Lazy point class
    Point(..),
    -- | Lazy perimeter function.
    perimeter,
    -- | Lazy doubleArea function.
    doubleArea,
    -- | Lazy plus function.
    plus,
    -- | Lazy minus function.
    minus,
    -- | Lazy scalarProduct function.
    scalarProduct,
    -- | Lazy crossProduct function.
    crossProduct
  )
where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)

-- | class for lazy Point
data Point = Point
  {
    x :: Int,
    y :: Int
  }
  deriving (Eq, Read, Show, NFData, Generic)

-- | plus function for lazy Point
plus :: Point -> Point -> Point
plus (Point x1 y1) (Point x2 y2) = (Point (x1 + x2) (y1 + y2))

-- | minus function for lazy Point
minus :: Point -> Point -> Point
minus (Point x1 y1) (Point x2 y2) = (Point (x1 - x2) (y1 - y2))

-- | scalarProduct function for lazy Point
scalarProduct :: Point -> Point -> Int
scalarProduct (Point x1 y1) (Point x2 y2) = x1 * x2 + y1 * y2

-- | crossProduct function for lazy Point
crossProduct :: Point -> Point -> Int
crossProduct (Point x1 y1) (Point x2 y2) = x1 * y2 - x2 * y1

-- | productCounter function for lazy Point, that is implementation for perimeter and doubleArea
productCounter :: (Num v, NFData v) => (Point -> Point -> v) -> [Point] -> v
productCounter _ [] = 0
productCounter func l@(h : _) = productCounterImpl func h l
  where
    productCounterImpl :: (Num v, NFData v) => (Point -> Point -> v) -> Point -> [Point] -> v
    productCounterImpl _ _ [] = 0
    productCounterImpl f p1 [p2] = f p1 p2
    productCounterImpl f p1 (p2 : (p3 : ps)) = ((+) $ f p2 p3) $ productCounterImpl f p1 (p3 : ps)

-- | l2 norm function for lazy Point
l2 :: Point -> Double
l2 (Point x y) = (sqrt . fromIntegral) $ (x * x + y * y)

-- | perimeter function for list of lazy Points
perimeter :: [Point] -> Double
perimeter = productCounter (\p1 p2 -> l2 $ minus p1 p2)

-- | doubleArea function for list of lazy Points
doubleArea :: [Point] -> Int
doubleArea = abs . productCounter (\p1 p2 -> (x p2 - x p1) * (y p2 + y p1))
