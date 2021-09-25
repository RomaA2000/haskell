module Task2Bench
  (
    mcBenchmark
  )
where

import Criterion.Main (Benchmark, bench, bgroup, nf)
import Task2

func1 :: Double -> Double
func1 x = 1 / tan x + 1 / (tan x) / (tan x) + 1 / (tan (x * x))

mcBenchmark :: Benchmark
mcBenchmark =
  bgroup
    "Monte-Carlo"
    [ bgroup
        "10^6"
        [ bench "par" $ nf (calculatorP 2 3 func1) 1000000,
          bench "seq" $ nf (calculator 2 3 func1) 1000000
        ]
    ]