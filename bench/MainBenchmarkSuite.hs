module Main (
    main
 ) where

import Criterion.Config
import Criterion.Main
import System.Random

import qualified Signal.Wavelet.ListBench  as L
import qualified Signal.Wavelet.Repa1Bench as R

main :: IO ()
main = newStdGen >>= defaultMainWith benchConfig (return ()) . benchmarks

benchmarks :: RandomGen g => g -> [Benchmark]
benchmarks gen =
  [
    bgroup "DWT" . (:[])  $ bcompare  
    [ 
      bench "Lists" $ nf   L.benchDwt (L.dataDwt gen)
    , bench "Repa"  $ whnf R.benchDwt (R.dataDwt gen)
    ]
  , bgroup "IDWT" . (:[])  $ bcompare  
    [ 
      bench "Lists" $ nf   L.benchIdwt (L.dataDwt gen)
    , bench "Repa"  $ whnf R.benchIdwt (R.dataDwt gen)
    ]
  ]

benchConfig :: Config
benchConfig = defaultConfig {
             cfgPerformGC = ljust True
           }
