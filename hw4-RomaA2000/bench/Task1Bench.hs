module Task1Bench
  (
    geomBenchmark
  )
where

import Control.DeepSeq (deepseq)
import Criterion.Main (Benchmark, bench, bgroup, nf)
import qualified Task1Perf as P
import qualified Task1Norm as N

geomBenchmark :: Benchmark
geomBenchmark =
  bgroup
    "strict geom bench"
    [ bgroup
        "perimeter 1000"
        [ kpNorm `deepseq` bench "normal" $ nf N.perimeter kpNorm
        , kpPerf `deepseq` bench "perf" $ nf P.perimeter kpPerf
        ]
    , bgroup
        "double area 1000"
        [ kpNorm `deepseq` bench "normal" $ nf N.doubleArea kpNorm
        , kpPerf `deepseq` bench "perf" $ nf P.doubleArea kpPerf
        ]
    , bgroup
        "perimeter 10000"
        [ tkpNorm `deepseq` bench "normal" $ nf N.perimeter tkpNorm
        , tkpPerf `deepseq` bench "perf" $ nf P.perimeter tkpPerf
        ]
    , bgroup
        "double area 10000"
        [ tkpNorm `deepseq` bench "normal" $ nf N.doubleArea tkpNorm
        , tkpPerf `deepseq` bench "perf" $ nf P.doubleArea tkpPerf
        ]
    ]
      where
        kpNorm  = replicate 1000  $ N.Point 0 0
        kpPerf = replicate 1000  $ P.Point 0 0
        tkpNorm = replicate 10000 $ N.Point 0 0
        tkpPerf  = replicate 10000 $ P.Point 0 0