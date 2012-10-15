module Main (
    main
 ) where

import Criterion.Main
import System.Random

import Signal.WaveletBench

main :: IO ()
main = newStdGen >>= defaultMain . benchmarks

--TODO enable GC between benchmarks
benchmarks :: RandomGen g => g -> [Benchmark]
benchmarks gen = 
   [ 
     bgroup "Lists"  
     [  
       bench "DWT" $ nf benchDwt (dataDwt gen)
     ] 
   ]
