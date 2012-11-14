module Main (
    main
 ) where

import Criterion.Config
import Criterion.Main
import System.Random

import qualified Signal.Wavelet.C1Bench      as C1
import qualified Signal.Wavelet.Eval1Bench   as E1
import qualified Signal.Wavelet.List1Bench   as L1
import qualified Signal.Wavelet.Repa1Bench   as R1
import qualified Signal.Wavelet.Repa2Bench   as R2
import qualified Signal.Wavelet.Vector1Bench as V1


main :: IO ()
main = return (mkStdGen 1232134332) >>= 
       defaultMainWith benchConfig (return ()) . benchmarks


benchmarks :: RandomGen g => g -> [Benchmark]
benchmarks gen =
    let lsSize    = 8
        sigSize   = 8192
        l1DataDwt = L1.dataDwt gen lsSize sigSize
        e1DataDwt = E1.dataDwt l1DataDwt
        v1DataDwt = V1.dataDwt l1DataDwt
        r1DataDwt = R1.dataDwt l1DataDwt
        r2DataDwt = R2.dataDwt l1DataDwt
        c1DataDwt = C1.dataDwt l1DataDwt
    in [
      bgroup "DWT" . (:[])  $ bcompare  
      [ 
        bench "Lists"  $ nf   L1.benchDwt  l1DataDwt
      , bench "Eval"   $ nf   E1.benchDwt  e1DataDwt
--      , bench "Vector" $ whnf V1.benchDwt v1DataDwt
--      , bench "Repa1"  $ whnf R1.benchDwt r1DataDwt
--      , bench "Repa2"  $ whnf R2.benchDwt r2DataDwt
--      , bench "C"      $ whnf C1.benchDwt c1DataDwt
      ]
{-    , bgroup "IDWT" . (:[])  $ bcompare  
      [ 
        bench "Lists"  $ nf   L1.benchIdwt l1DataDwt
        bench "Eval"   $ nf   E1.benchIdwt e1DataDwt
      , bench "Vector" $ whnf V1.benchIdwt v1DataDwt
      , bench "Repa1"  $ whnf R1.benchIdwt r1DataDwt
      , bench "Repa2"  $ whnf R2.benchIdwt r2DataDwt
      , bench "C"      $ whnf C1.benchIdwt c1DataDwt
      ]-}
    ]


benchConfig :: Config
benchConfig = defaultConfig {
             cfgPerformGC = ljust True
           }
