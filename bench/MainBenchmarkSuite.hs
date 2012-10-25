module Main (
    main
 ) where

import Criterion.Config
import Criterion.Main
import System.Random

import qualified Signal.Wavelet.CBench     as C
import qualified Signal.Wavelet.ListBench  as L
import qualified Signal.Wavelet.Repa1Bench as R


main :: IO ()
main = return (mkStdGen 1232134332) >>= 
       defaultMainWith benchConfig (return ()) . benchmarks


benchmarks :: RandomGen g => g -> [Benchmark]
benchmarks gen =
    let lsSize   = 8
        sigSize  = 8192
        lDataDwt = L.dataDwt gen lsSize sigSize
        rDataDwt = R.dataDwt gen lsSize sigSize
        cDataDwt = C.dataDwt gen lsSize sigSize
    in [
      bgroup "DWT" . (:[])  $ bcompare  
      [ 
        bench "Lists" $ nf   L.benchDwt lDataDwt
      , bench "Repa"  $ whnf R.benchDwt rDataDwt
      , bench "C"     $ whnf C.benchDwt cDataDwt
      ]
    , bgroup "IDWT" . (:[])  $ bcompare  
      [ 
        bench "Lists" $ nf   L.benchIdwt lDataDwt
      , bench "Repa"  $ whnf R.benchIdwt rDataDwt
      , bench "C"     $ whnf C.benchIdwt cDataDwt
      ]
    ]


benchConfig :: Config
benchConfig = defaultConfig {
             cfgPerformGC = ljust True
           }
