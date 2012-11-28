module Main (
    main
 ) where

import Criterion.Config
import Criterion.Main
import System.Random

import qualified Signal.Wavelet.C1Bench          as C1
--import qualified Signal.Wavelet.Eval.CommonBench as EC
--import qualified Signal.Wavelet.Eval1Bench       as E1
--import qualified Signal.Wavelet.Eval2Bench       as E2
--import qualified Signal.Wavelet.List.CommonBench as LC
import qualified Signal.Wavelet.List1Bench       as L1
--import qualified Signal.Wavelet.List2Bench       as L2
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
    in [ -- See Note [C/FFI criterion bug]
     bgroup "Lattice" 
      [
        bench "C1 Seq"          $ whnf C1.benchLattice  cDataLattice      
      , bench "Vector1 Seq"     $ whnf V1.benchLattice  vDataLattice      
      , bench "Repa1 Seq"       $ whnf R1.benchLatticeS rDataLattice
      , bench "Repa1 Par"       $ whnf R1.benchLatticeP rDataLattice
      , bench "Repa2 Seq"       $ whnf R2.benchLatticeS rDataLattice
      , bench "Repa2 Par"       $ whnf R2.benchLatticeP rDataLattice
      , bench "Repa3 Seq"       $ whnf R3.benchLatticeS rDataLattice
      , bench "Repa3 Par"       $ whnf R3.benchLatticeP rDataLattice
--    , bench "List.Common Seq" $   nf LC.benchLattice lDataLattice
--    , bench "Eval.Common Par" $   nf EC.benchLattice lDataLattice
      ]
   , bgroup "DWT" . (:[])  $ bcompare  
      [ 
        bench "C1 Seq"      $ whnf C1.benchDwt  cDataDwt
      , bench "Vector1 Seq" $ whnf V1.benchDwt  vDataDwt
      , bench "Repa1 Seq"   $ whnf R1.benchDwtS rDataDwt
      , bench "Repa1 Par"   $ whnf R1.benchDwtP rDataDwt
      , bench "Repa2 Seq"   $ whnf R2.benchDwtS rDataDwt
      , bench "Repa2 Par"   $ whnf R2.benchDwtP rDataDwt
--      , bench "List1"   $ nf   L1.benchDwt lDataDwt
--      , bench "List2"   $ nf   L2.benchDwt lDataDwt
--      , bench "Eval1"   $ nf   E1.benchDwt lDataDwt
--      , bench "Eval2"   $ nf   E2.benchDwt lDataDwt
      ]
{-   , bgroup "IDWT" . (:[])  $ bcompare  
      [ 
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
        bench "Repa2 Seq"   $ whnf R2.benchExtendFrontS rDataExtend
        bench "Repa2 Par"   $ whnf R2.benchExtendFrontP rDataExtend
      , bench "List.Common" $   nf LC.benchExtendFront  lDataExtend
      ]-}
   , bgroup "ExtendEnd" 
      [
        bench "Repa2 Seq"   $ whnf R2.benchExtendEndS rDataExtend
      , bench "Repa2 Par"   $ whnf R2.benchExtendEndP rDataExtend
--    , bench "List.Common" $   nf LC.benchExtendEnd  lDataExtend
      ]
    ]


benchConfig :: Config
benchConfig = defaultConfig {
             cfgPerformGC = ljust True
           }


{-

Note [C/FFI criterion bug]
~~~~~~~~~~~~~~~~~~~~~~~~~~
When benchmarking C bindings with criterion the first benchmark returns 
correct result. All other benchmarks that use FFI estimate run time 
to be longer. This does not happen always and seems to depend on CPU. These
are possibly cache effects.

-}
