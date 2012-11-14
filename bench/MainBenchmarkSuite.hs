module Main (
    main
 ) where

import Criterion.Config
import Criterion.Main
import System.Random

import qualified Signal.Wavelet.C1Bench      as C1
import qualified Signal.Wavelet.Eval1Bench   as E1
import qualified Signal.Wavelet.Eval2Bench   as E2
import qualified Signal.Wavelet.List1Bench   as L1
import qualified Signal.Wavelet.List2Bench   as L2
import qualified Signal.Wavelet.Repa1Bench   as R1
import qualified Signal.Wavelet.Repa2Bench   as R2
import qualified Signal.Wavelet.Vector1Bench as V1


main :: IO ()
main = return (mkStdGen 1232134332) >>= 
       defaultMainWith benchConfig (return ()) . benchmarks


benchmarks :: RandomGen g => g -> [Benchmark]
benchmarks gen =
    let lsSize   = 8
        sigSize  = 8192
        lDataDwt = L1.dataDwt gen lsSize sigSize
--        cDataDwt = C1.dataDwt lDataDwt
--        rDataDwt = R1.dataDwt lDataDwt
--        vDataDwt = V1.dataDwt lDataDwt
    in [
      bgroup "DWT" . (:[])  $ bcompare  
      [ 
--        bench "C1"      $ whnf C1.benchDwt cDataDwt
--      , bench "Vector1" $ whnf V1.benchDwt vDataDwt
--      , bench "Repa1"   $ whnf R1.benchDwt rDataDwt
--      , bench "Repa2"   $ whnf R2.benchDwt rDataDwt
--        bench "List1"   $ nf   L1.benchDwt lDataDwt
       bench "List2"   $ nf   L2.benchDwt lDataDwt
--      , bench "Eval1"   $ nf   E1.benchDwt lDataDwt
      , bench "Eval2"   $ nf   E2.benchDwt lDataDwt
      ]
    , bgroup "IDWT" . (:[])  $ bcompare  
      [ 
--        bench "C1"      $ whnf C1.benchIdwt cDataDwt
--      , bench "Vector1" $ whnf V1.benchIdwt vDataDwt
--      , bench "Repa1"   $ whnf R1.benchIdwt rDataDwt
--      , bench "Repa2"   $ whnf R2.benchIdwt rDataDwt
--        bench "List1"   $ nf   L1.benchIdwt lDataDwt
       bench "List2"   $ nf   L2.benchIdwt lDataDwt
--      , bench "Eval1"   $ nf   E1.benchIdwt lDataDwt
      , bench "Eval2"   $ nf   E2.benchIdwt lDataDwt
      ]
    ]


benchConfig :: Config
benchConfig = defaultConfig {
             cfgPerformGC = ljust True
           }
