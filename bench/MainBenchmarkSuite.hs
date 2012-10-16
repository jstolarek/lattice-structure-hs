module Main (
    main
 ) where

import Criterion.Config
import Criterion.Main
import System.Random

import Signal.WaveletBench

main :: IO ()
main = newStdGen >>= defaultMainWith benchConfig (return ()) . benchmarks

benchmarks :: RandomGen g => g -> [Benchmark]
benchmarks gen =
  [ 
   bgroup "Lists"  
    [ 
     bench "DWT" $ nf benchDwt (dataDwt gen) 
    ] 
  ]

benchConfig :: Config
benchConfig = defaultConfig {
             cfgPerformGC = ljust True
           }
