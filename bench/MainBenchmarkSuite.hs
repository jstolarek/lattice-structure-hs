module Main (
    main
 ) where

import Criterion.Config
import Criterion.Main
import System.Random

import qualified Signal.WaveletBench as L
import qualified Signal.Repa.WaveletBench as R

main :: IO ()
main = newStdGen >>= defaultMainWith benchConfig (return ()) . benchmarks

benchmarks :: RandomGen g => g -> [Benchmark]
benchmarks gen =
  [
    bgroup "DWT" . (:[])  $ bcompare  
    [ 
--      bench "Lists" $ nf   L.benchDwt (L.dataDwt gen)
     bench "Repa"  $ whnf R.benchDwt (R.dataDwt gen)
    ]
  ]

benchConfig :: Config
benchConfig = defaultConfig {
             cfgPerformGC = ljust True
           }
