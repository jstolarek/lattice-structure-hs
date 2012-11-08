module Main (
    main
 ) where

import Criterion.Config
import Criterion.Main
import System.Random

import qualified Signal.Wavelet.CBench      as C
import qualified Signal.Wavelet.EvalBench   as E
import qualified Signal.Wavelet.ListBench   as L
import qualified Signal.Wavelet.Repa1Bench  as R1
import qualified Signal.Wavelet.Repa2Bench  as R2
import qualified Signal.Wavelet.VectorBench as V


main :: IO ()
main = return (mkStdGen 1232134332) >>= 
       defaultMainWith benchConfig (return ()) . benchmarks


benchmarks :: RandomGen g => g -> [Benchmark]
benchmarks gen =
    let lsSize    = 8
        sigSize   = 8192
        lDataDwt  =  L.dataDwt gen lsSize sigSize
        eDataDwt  =  E.dataDwt lDataDwt
        vDataDwt  =  V.dataDwt lDataDwt
        r1DataDwt = R1.dataDwt lDataDwt
        r2DataDwt = R2.dataDwt lDataDwt
        cDataDwt  =  C.dataDwt lDataDwt
    in [
      bgroup "DWT" . (:[])  $ bcompare  
      [ 
        bench "Lists" $ nf   L.benchDwt lDataDwt
      , bench "Eval"   $   nf  E.benchDwt  eDataDwt
--      , bench "Vector" $ whnf  V.benchDwt  vDataDwt
--      , bench "Repa1"  $ whnf R1.benchDwt r1DataDwt
--      , bench "Repa2"  $ whnf R2.benchDwt r2DataDwt
--      , bench "C"      $ whnf  C.benchDwt  cDataDwt
      ]
{-    , bgroup "IDWT" . (:[])  $ bcompare  
      [ 
        bench "Lists" $ nf   L.benchIdwt lDataDwt
        bench "Eval"   $   nf  E.benchIdwt  eDataDwt
      , bench "Vector" $ whnf  V.benchIdwt  vDataDwt
      , bench "Repa1"  $ whnf R1.benchIdwt r1DataDwt
      , bench "Repa2"  $ whnf R2.benchIdwt r2DataDwt
      , bench "C"      $ whnf  C.benchIdwt  cDataDwt
      ]-}
    ]


benchConfig :: Config
benchConfig = defaultConfig {
             cfgPerformGC = ljust True
           }
