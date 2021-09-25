{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-
Module : Task1Perf
Functions for strict operations with points.
-}
module Task1Perf
  (
    -- | Strict point class.
    Point(..),
    -- | Strict perimeter function.
    perimeter,
    -- | Strict doubleArea function.
    doubleArea,
    -- | Strict plus function.
    plus,
    -- | Strict minus function.
    minus,
    -- | Strict scalarProduct function.
    scalarProduct,
    -- | Strict crossProduct function.
    crossProduct
  )
where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)

-- | class for strict Point
data Point = Point
  {
    x :: {-# UNPACK #-} !Int,
    y :: {-# UNPACK #-} !Int
  }
  deriving (Eq, Read, Show, NFData, Generic)

-- | plus function for strict Point
plus :: Point -> Point -> Point
plus (Point x1 y1) (Point x2 y2) = (Point (x1 + x2) (y1 + y2))

-- | minus function for strict Point
minus :: Point -> Point -> Point
minus (Point x1 y1) (Point x2 y2) = (Point (x1 - x2) (y1 - y2))

-- | scalarProduct function for strict Point
scalarProduct :: Point -> Point -> Int
scalarProduct (Point x1 y1) (Point x2 y2) = x1 * x2 + y1 * y2

-- | crossProduct function for strict Point
crossProduct :: Point -> Point -> Int
crossProduct (Point x1 y1) (Point x2 y2) = x1 * y2 - x2 * y1

-- | productCounter function for strict Point, that is implementation for perimeter and doubleArea
productCounter :: (Num v, NFData v) => (Point -> Point -> v) -> [Point] -> v
productCounter _ [] = 0
productCounter !func l@(h : _) = productCounterImpl func h l
  where
    productCounterImpl :: (Num v, NFData v) => (Point -> Point -> v) -> Point -> [Point] -> v
    productCounterImpl _ _ [] = 0
    productCounterImpl f p1 [p2] = f p1 p2
    productCounterImpl f p1 (p2 : (p3 : ps)) = ((+) $! f p2 p3) $! productCounterImpl f p1 (p3 : ps)

-- | l2 norm function for strict Point
l2 :: Point -> Double
l2 (Point x y) = (sqrt . fromIntegral) $! (x * x + y * y)

-- | perimeter function for list of strict Points
perimeter :: [Point] -> Double
perimeter = productCounter (\p1 p2 -> l2 $! minus p1 p2)

-- | doubleArea function for list of strict Points
doubleArea :: [Point] -> Int
doubleArea = abs . productCounter (\p1 p2 -> (x p2 - x p1) * (y p2 + y p1))
