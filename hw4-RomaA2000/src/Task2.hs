{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-
Module : Task2.
Functions for monte-carlo method.
-}
module Task2
  (
    -- | calculatorP parallel function.
    calculatorP,
    -- | calculator sequential function.
    calculator
  )
where

import System.Random(mkStdGen, randomRs, randomR)
import Control.Monad.Par.Combinator(InclusiveRange(InclusiveRange), parMapReduceRange)
import Control.Monad.Par(runPar)
import Data.List

-- | Returns random double from interval.
number :: Double -> Double -> Int -> Double
number a b s = do
  let pureGen = mkStdGen s
  fst (randomR (a, b) pureGen)

-- | Returns list of random doubles from interval.
numbersGenerator :: Double -> Double -> Int -> [Double]
numbersGenerator a b n = do
  let pureGen = mkStdGen 239
  take n (randomRs (a, b) pureGen)

-- | Calculates integral parallel on interval.
calculatorP :: Double -> Double -> (Double -> Double) -> Int -> Double
calculatorP a b f n = do
  let size = (b - a) / (fromIntegral n)
  let sumResult = runPar $ parMapReduceRange (InclusiveRange 0 (n-1)) (\x -> return $ f ((fromIntegral x) * size + a)) (\x y -> return (x + y)) 0
  sumResult * (b - a) / (fromIntegral (n + 1))

-- | Calculates integral sequentially on interval.
calculator :: Double -> Double -> (Double -> Double) -> Int -> Double
calculator a b f n = do
  let numList = numbersGenerator a b n
  let vals = map f numList
  let sumResult = foldl' ((+) :: Double -> Double -> Double) (0 :: Double) vals
  sumResult * (b - a) / (fromIntegral n)
