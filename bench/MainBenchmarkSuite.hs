module Main (
    main
 ) where

import Criterion.Config
import Criterion.Main
import System.Random

import qualified Signal.Wavelet.C1Bench          as C1
import qualified Signal.Wavelet.Eval.CommonBench as EC
import qualified Signal.Wavelet.Eval1Bench       as E1
import qualified Signal.Wavelet.Eval2Bench       as E2
import qualified Signal.Wavelet.List.CommonBench as LC
import qualified Signal.Wavelet.List1Bench       as L1
import qualified Signal.Wavelet.List2Bench       as L2
import qualified Signal.Wavelet.Repa1Bench       as R1
import qualified Signal.Wavelet.Repa2Bench       as R2
import qualified Signal.Wavelet.Repa3Bench       as R3
import qualified Signal.Wavelet.Vector1Bench     as V1


main :: IO ()
main = return (mkStdGen 1232134332) >>= 
       defaultMainWith benchConfig (return ()) . benchmarks


benchmarks :: RandomGen g => g -> [Benchmark]
benchmarks gen =
    let lsSize       = 8
        sigSize      = 8192
        lDataDwt     = L1.dataDwt gen lsSize sigSize
        cDataDwt     = C1.dataDwt lDataDwt
        rDataDwt     = R1.dataDwt lDataDwt
        vDataDwt     = V1.dataDwt lDataDwt
--      lDataLattice = LC.dataLattice lDataDwt
        cDataLattice = C1.dataLattice lDataDwt
        rDataLattice = R1.dataLattice lDataDwt
        vDataLattice = V1.dataLattice lDataDwt
--      lDataExtend  = LC.dataExtend lDataDwt
        rDataExtend  = R2.dataExtend lDataDwt
        rDataDistrib = R3.dataDistributeWork lDataDwt
    in [
     bgroup "Lattice" 
      [
        bench "C1"          $ whnf C1.benchLattice cDataLattice
      , bench "Vector1"     $ whnf V1.benchLattice vDataLattice
      , bench "Repa1"       $ whnf R1.benchLattice rDataLattice
      , bench "Repa2"       $ whnf R2.benchLattice rDataLattice
      , bench "Repa3"       $ whnf R3.benchLattice rDataLattice
--      , bench "List.Common" $   nf LC.benchLattice lDataLattice
--      , bench "Eval.Common" $   nf EC.benchLattice lDataLattice
      ]
{-   , bgroup "DWT" . (:[])  $ bcompare  
      [ 
        bench "C1"      $ whnf C1.benchDwt cDataDwt
      , bench "Vector1" $ whnf V1.benchDwt vDataDwt
      , bench "Repa1"   $ whnf R1.benchDwt rDataDwt
      , bench "Repa2"   $ whnf R2.benchDwt rDataDwt
--      , bench "List1"   $ nf   L1.benchDwt lDataDwt
--      , bench "List2"   $ nf   L2.benchDwt lDataDwt
--      , bench "Eval1"   $ nf   E1.benchDwt lDataDwt
--      , bench "Eval2"   $ nf   E2.benchDwt lDataDwt
      ]-}
{-   , bgroup "IDWT" . (:[])  $ bcompare  
      [ -- See Note [C criterion bug]
        bench "C1"      $ whnf C1.benchIdwt cDataDwt 
      , bench "Vector1" $ whnf V1.benchIdwt vDataDwt
      , bench "Repa1"   $ whnf R1.benchIdwt rDataDwt
      , bench "Repa2"   $ whnf R2.benchIdwt rDataDwt
--      , bench "List1"   $ nf   L1.benchIdwt lDataDwt
--      , bench "List2"   $ nf   L2.benchIdwt lDataDwt
--      , bench "Eval1"   $ nf   E1.benchIdwt lDataDwt
--      , bench "Eval2"   $ nf   E2.benchIdwt lDataDwt
      ]
   , bgroup "ExtendFront"
      [
        bench "Repa2"       $ whnf R2.benchExtendFront rDataExtend
      , bench "List.Common" $   nf LC.benchExtendFront lDataExtend
      ]-}
   , bgroup "ExtendEnd" 
      [
        bench "Repa2"       $ whnf R2.benchExtendEnd   rDataExtend
--      , bench "List.Common" $   nf LC.benchExtendEnd   lDataExtend
      ]
   , bgroup "Other" 
      [
        bench "Repa3/distributeWork" $ whnf R3.benchDistributeWork rDataDistrib
      ]
    ]


benchConfig :: Config
benchConfig = defaultConfig {
             cfgPerformGC = ljust True
           }


{-
Note [C criterion bug]
~~~~~~~~~~~~~~~~~~~~~~
When benchmarking C bindings with criterion the first benchmark returns 
correct result. All other benchmarks that use FFI estimate run time 
to be longer. This does not happen always and seems to depend on CPU. These
are possibly cache effects.

-}
