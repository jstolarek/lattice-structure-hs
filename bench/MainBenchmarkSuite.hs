module Main (
    main
 ) where

import Criterion.Config
import Criterion.Main
import System.Random

import qualified Signal.Wavelet.C1Bench           as C1
import qualified Signal.Wavelet.Eval.CommonBench  as EC
import qualified Signal.Wavelet.Eval1Bench        as E1
import qualified Signal.Wavelet.Eval2Bench        as E2
import qualified Signal.Wavelet.List.CommonBench  as LC
import qualified Signal.Wavelet.List1Bench        as L1
import qualified Signal.Wavelet.List2Bench        as L2
import qualified Signal.Wavelet.Repa1Bench        as R1
import qualified Signal.Wavelet.Repa2Bench        as R2
import qualified Signal.Wavelet.Repa3Bench        as R3
import qualified Signal.Wavelet.Vector1Bench      as V1
import qualified Signal.Wavelet.Repa.LibraryBench as RL


main :: IO ()
main = return (mkStdGen 1232134332) >>=
       defaultMainWith benchConfig (return ()) . benchmarks


benchmarks :: RandomGen g => g -> [Benchmark]
benchmarks gen =
    let lsSize         = 6
        sigSize        = 2 * 8192
        lDataDwt       = L1.dataDwt gen lsSize sigSize
        cDataDwt       = C1.dataDwt lDataDwt
        rDataDwt       = R1.dataDwt lDataDwt
        vDataDwt       = V1.dataDwt lDataDwt
        cDataLattice   = C1.dataLattice   lDataDwt
        vDataLattice   = V1.dataLattice   lDataDwt
        rDataLattice   = R1.dataLattice   lDataDwt
        rDataToPairs   = R1.dataToPairs   lDataDwt
        rDataFromPairs = R1.dataFromPairs lDataDwt
        rDataCslCsr    = R1.dataCslCsr    lDataDwt
        rDataCslNCsrN  = R1.dataCslNCsrN  lDataDwt
        rDataExtend    = R2.dataExtend    lDataDwt
        r3DataLattice  = R3.dataLattice   lDataDwt
        lDataLattice   = LC.dataLattice   lDataDwt
        lDataExtend    = LC.dataExtend    lDataDwt
        rDataCompute   = RL.dataCompute   lDataDwt
        rDataCopy      = RL.dataCopy      lDataDwt
        rDataExtract   = RL.dataExtract   lDataDwt
        rDataAppend    = RL.dataAppend    lDataDwt
        rDataBckperm   = RL.dataBckperm   lDataDwt
        rDataMap       = RL.dataMap       lDataDwt
        rDataTraverse  = RL.dataTraverse  lDataDwt
    in [ -- See: Note [C/FFI criterion bug]
     bgroup "DWT" . (:[]) $ bcompare
      [
        bench "C1 Seq"          $ whnf C1.benchDwt  cDataDwt
      , bench "Vector1 Seq"     $ whnf V1.benchDwt  vDataDwt
      , bench "Repa1 Seq"       $ whnf R1.benchDwtS rDataDwt
      , bench "Repa1 Par"       $ whnf R1.benchDwtP rDataDwt
      , bench "Repa2 Seq"       $ whnf R2.benchDwtS rDataDwt
      , bench "Repa2 Par"       $ whnf R2.benchDwtP rDataDwt
      , bench "Repa3 Seq"       $ whnf R3.benchDwtS rDataDwt
      , bench "Repa3 Par"       $ whnf R3.benchDwtP rDataDwt
      , bench "List1 Seq"       $ nf   L1.benchDwt  lDataDwt
      , bench "List2 Seq"       $ nf   L2.benchDwt  lDataDwt
      , bench "Eval1 Par"       $ nf   E1.benchDwt  lDataDwt
      , bench "Eval2 Par"       $ nf   E2.benchDwt  lDataDwt
      ]
   , bgroup "IDWT" . (:[]) $ bcompare
      [
        bench "C1 Seq"          $ whnf C1.benchIdwt  cDataDwt
      , bench "Vector1 Seq"     $ whnf V1.benchIdwt  vDataDwt
      , bench "Repa1 Seq"       $ whnf R1.benchIdwtS rDataDwt
      , bench "Repa1 Par"       $ whnf R1.benchIdwtP rDataDwt
      , bench "Repa2 Seq"       $ whnf R2.benchIdwtS rDataDwt
      , bench "Repa2 Par"       $ whnf R2.benchIdwtP rDataDwt
      , bench "Repa3 Seq"       $ whnf R3.benchIdwtS rDataDwt
      , bench "Repa3 Par"       $ whnf R3.benchIdwtP rDataDwt
      , bench "List1 Seq"       $ nf   L1.benchIdwt  lDataDwt
      , bench "List2 Seq"       $ nf   L2.benchIdwt  lDataDwt
      , bench "Eval1 Par"       $ nf   E1.benchIdwt  lDataDwt
      , bench "Eval2 Par"       $ nf   E2.benchIdwt  lDataDwt
      ]
   , bgroup "C1"
      [
        bench "Lattice Seq"     $ whnf C1.benchLattice cDataLattice
      ]
   , bgroup "Vector1"
      [
        bench "Lattice Seq"     $ whnf V1.benchLattice vDataLattice
      ]
   , bgroup "Repa1"
      [
        bench "Lattice Seq"     $ whnf R1.benchLatticeS    rDataLattice
      , bench "Lattice Par"     $ whnf R1.benchLatticeP    rDataLattice
      , bench "ToPairs Seq"     $ whnf R1.benchToPairsS    rDataToPairs
      , bench "ToPairs Par"     $ whnf R1.benchToPairsP    rDataToPairs
      , bench "FromPairs Seq"   $ whnf R1.benchFromPairsS  rDataFromPairs
      , bench "FromPairs Par"   $ whnf R1.benchFromPairsP  rDataFromPairs
      , bench "Csl Seq"         $ whnf R1.benchCslS        rDataCslCsr
      , bench "Csl Par"         $ whnf R1.benchCslP        rDataCslCsr
      , bench "CslP Seq"        $ whnf R1.benchCslSP       rDataCslCsr
      , bench "CslP Par"        $ whnf R1.benchCslPP       rDataCslCsr
      , bench "Csr Seq"         $ whnf R1.benchCsrS        rDataCslCsr
      , bench "Csr Par"         $ whnf R1.benchCsrP        rDataCslCsr
      , bench "CsrP Seq"        $ whnf R1.benchCsrSP       rDataCslCsr
      , bench "CsrP Par"        $ whnf R1.benchCsrPP       rDataCslCsr
      , bench "CslN Seq"        $ whnf R1.benchCslNS       rDataCslNCsrN
      , bench "CslN Par"        $ whnf R1.benchCslNP       rDataCslNCsrN
      , bench "CsrN Seq"        $ whnf R1.benchCsrNS       rDataCslNCsrN
      , bench "CsrN Par"        $ whnf R1.benchCsrNP       rDataCslNCsrN
      , bench "Lat+Frc+Csl Seq" $ whnf R1.benchLatticeForceCslS rDataLattice
      , bench "Lat+Frc+Csl Par" $ whnf R1.benchLatticeForceCslP rDataLattice
      , bench "Lattice+Csl Seq" $ whnf R1.benchLatticeCslS rDataLattice
      , bench "Lattice+Csl Par" $ whnf R1.benchLatticeCslP rDataLattice
      ]
   , bgroup "Repa2"
      [
        bench "Lattice Seq"     $ whnf R2.benchLatticeS     rDataLattice
      , bench "Lattice Par"     $ whnf R2.benchLatticeP     rDataLattice
      , bench "Trim+lattice Seq"$ whnf R2.benchTrimLatticeS rDataLattice
      , bench "Trim+lattice Par"$ whnf R2.benchTrimLatticeP rDataLattice
      , bench "ExtendFront Seq" $ whnf R2.benchExtendFrontS rDataExtend
      , bench "ExtendFront Par" $ whnf R2.benchExtendFrontP rDataExtend
      , bench "ExtendEnd Seq"   $ whnf R2.benchExtendEndS   rDataExtend
      , bench "ExtendEnd Par"   $ whnf R2.benchExtendEndP   rDataExtend
      ]
   , bgroup "Repa3"
      [
        bench "Lattice Seq"     $ whnf R3.benchLatticeS r3DataLattice
      , bench "Lattice Par"     $ whnf R3.benchLatticeP r3DataLattice
      ]
   , bgroup "List.Common"
      [
        bench "Lattice Seq"     $   nf LC.benchLattice     lDataLattice
      , bench "ExtendFront Seq" $   nf LC.benchExtendFront lDataExtend
      , bench "ExtendEnd Seq"   $   nf LC.benchExtendEnd   lDataExtend
      ]
    , bgroup "Eval.Common"
      [
        bench "Lattice Par"     $   nf EC.benchLattice lDataLattice
      ]
   , bgroup "Repa built-in functions"
      [
        bench "computeS"           $ whnf    RL.benchComputeS  rDataCompute
      , bench "computeP"           $ whnfIO (RL.benchComputeP  rDataCompute)
      , bench "copyS"              $ whnf    RL.benchCopyS     rDataCopy
      , bench "copyP"              $ whnfIO (RL.benchCopyP     rDataCopy)
      , bench "extractS"           $ whnf    RL.benchExtractS  rDataExtract
      , bench "extractP"           $ whnfIO (RL.benchExtractP  rDataExtract)
      , bench "appendS"            $ whnf    RL.benchAppendS   rDataAppend
      , bench "appendP"            $ whnfIO (RL.benchAppendP   rDataAppend)
      , bench "backpermuteS"       $ whnf    RL.benchBckpermS  rDataBckperm
      , bench "backpermuteP"       $ whnfIO (RL.benchBckpermP  rDataBckperm)
      , bench "mapS"               $ whnf    RL.benchMapS      rDataMap
      , bench "mapP"               $ whnfIO (RL.benchMapP      rDataMap)
      , bench "traverseS"          $ whnf    RL.benchTraverseS rDataTraverse
      , bench "traverseP"          $ whnfIO (RL.benchTraverseP rDataTraverse)
      ]
    ]


benchConfig :: Config
benchConfig = defaultConfig {
             cfgPerformGC = ljust True
           }


-- Note [C/FFI criterion bug]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- When benchmarking C bindings with criterion the first benchmark returns
-- correct result. All other benchmarks that use FFI estimate run time to be
-- longer. This does not happen always and seems to depend on CPU and size of
-- processed data. These are possibly cache effects. This bug does not occur on
-- some machines. If you observe any of below it means your results are affected
-- by the bug:
--
-- a) time needed to run IDWT/C1 benchmark is significantly longer than DWT/C1
-- b) C1/Lattice takes longer than Vector1/Lattice
