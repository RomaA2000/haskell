module Main
  ( main
  ) where

import Criterion.Main (defaultMain)
import Task1Bench
import Task2Bench

main :: IO ()
main =
  defaultMain
    [ mcBenchmark
    , geomBenchmark
    ]