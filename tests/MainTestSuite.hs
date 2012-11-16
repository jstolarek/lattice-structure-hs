module Main ( 
    main 
 ) where

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Utils

import qualified Signal.Wavelet.C1Test          as C1
import qualified Signal.Wavelet.Eval.CommonTest as EC
import qualified Signal.Wavelet.Eval1Test       as E1
import qualified Signal.Wavelet.Eval2Test       as E2
import qualified Signal.Wavelet.List.CommonTest as LC
import qualified Signal.Wavelet.List1Test       as L1
import qualified Signal.Wavelet.List2Test       as L2
import qualified Signal.Wavelet.Repa.CommonTest as RC
import qualified Signal.Wavelet.Repa1Test       as R1
import qualified Signal.Wavelet.Repa2Test       as R2
import qualified Signal.Wavelet.Vector1Test     as V1

import qualified Signal.Wavelet.ReferenceITest  as RF

main :: IO ()
main = defaultMain tests


tests :: [Test]
tests = [ 
   testGroup "List common" [
      testProvider "Lattice layer"               LC.testLattice LC.dataLattice
    , testProperty "Inverting lattice layer"     LC.propDoubleLatticeIdentity
    , testProvider "Extend front of signal"      LC.testExtendFront
                                                 LC.dataExtendFront
    , testProvider "Extend end of signal"        LC.testExtendEnd
                                                 LC.dataExtendEnd
    , testProperty "Inverting lattice structure" LC.propLatticeInverseInverse
    , testProperty "Deg-Rad identity"            LC.propDegRadInvertible
    , testProperty "Rad-Deg identity"            LC.propRadDegInvertible
    , testProvider "Cyclic shift left"           LC.testCsl  LC.dataCsl
    , testProvider "Cyclic shift right"          LC.testCsr  LC.dataCsr
    , testProvider "Cyclic shift left by N"      LC.testCslN LC.dataCslN
    , testProvider "Cyclic shift right by N"     LC.testCsrN LC.dataCsrN
    , testProperty "L/R shift composition"       LC.propIdentityShift1
    , testProperty "R/L shift composition"       LC.propIdentityShift2
    , testProperty "L/R shift by N composition"  LC.propIdentityShift3
    , testProperty "R/L shift by N composition"  LC.propIdentityShift4
    , testProperty "Periodic left shift"         LC.propIdentityShift5
    , testProperty "Periodic right shift"        LC.propIdentityShift6
  ],
  testGroup "List1" [ 
      testProvider "DWT"                         L1.testDwt  L1.dataDwt
    , testProvider "IDWT"                        L1.testIdwt L1.dataIdwt
    , testProperty "DWT-IDWT identity"           L1.propDWTInvertible
  ],
  testGroup "List2" [ 
      testProvider "DWT"                         L2.testDwt  L2.dataDwt
    , testProvider "IDWT"                        L2.testIdwt L2.dataIdwt
    , testProperty "DWT-IDWT identity"           L2.propDWTInvertible
  ],
  testGroup "Eval1" [ 
      testProvider "DWT"                         E1.testDwt  E1.dataDwt
    , testProvider "IDWT"                        E1.testIdwt E1.dataIdwt
    , testProperty "DWT-IDWT identity"           E1.propDWTInvertible
  ],
  testGroup "Eval2" [ 
      testProvider "DWT"                         E2.testDwt  E2.dataDwt
    , testProvider "IDWT"                        E2.testIdwt E2.dataIdwt
    , testProperty "DWT-IDWT identity"           E2.propDWTInvertible
  ],
  testGroup "Eval common" [
      testProperty "Lattice Eval like List"      EC.propLatticeIdenticalToList
  ],
  testGroup "Vector1" [
      testProvider "DWT"                         V1.testDwt     V1.dataDwt
    , testProvider "IDWT"                        V1.testIdwt    V1.dataIdwt
    , testProperty "DWT-IDWT identity"           V1.propDWTInvertible
    , testProvider "Lattice layer"               V1.testLattice V1.dataLattice
    , testProperty "Inverting lattice layer"     V1.propDoubleLatticeIdentity
  ],
  testGroup "Repa common" [
      testProperty "Deg-Rad identity"            RC.propDegRadInvertible
    , testProperty "Rad-Deg identity"            RC.propRadDegInvertible
    , testProperty "Inverting lattice structure" RC.propLatticeInverseInverse
  ],
  testGroup "Repa1" [
      testProvider "DWT"                         R1.testDwt  R1.dataDwt
    , testProvider "IDWT"                        R1.testIdwt R1.dataIdwt
    , testProperty "DWT-IDWT identity"           R1.propDWTInvertible
    , testProvider "Lattice layer"               R1.testLattice R1.dataLattice
    , testProperty "Inverting lattice layer"     R1.propDoubleLatticeIdentity
    , testProvider "Cyclic shift left"           R1.testCsl  R1.dataCsl
    , testProvider "Cyclic shift right"          R1.testCsr  R1.dataCsr
    , testProvider "Cyclic shift left by N"      R1.testCslN R1.dataCslN
    , testProvider "Cyclic shift right by N"     R1.testCsrN R1.dataCsrN
    , testProperty "L/R shift composition"       R1.propIdentityShift1
    , testProperty "R/L shift composition"       R1.propIdentityShift2
    , testProperty "L/R shift by N composition"  R1.propIdentityShift3
    , testProperty "R/L shift by N composition"  R1.propIdentityShift4
    , testProperty "Periodic left shift"         R1.propIdentityShift5
    , testProperty "Periodic right shift"        R1.propIdentityShift6
    , testProperty "List-Pairs-List identity"    R1.propPairsIdentity1
    , testProperty "Pairs-List-Pairs identity"   R1.propPairsIdentity2
  ],
  testGroup "Repa2" [
      testProvider "DWT"                         R2.testDwt  R2.dataDwt
    , testProvider "IDWT"                        R2.testIdwt R2.dataIdwt
    , testProperty "DWT-IDWT identity"           R2.propDWTInvertible
    , testProvider "Lattice layer"               R2.testLattice R2.dataLattice
    , testProperty "Inverting lattice layer"     R2.propDoubleLatticeIdentity
    , testProvider "Extend front of signal"      R2.testExtendFront
                                                 R2.dataExtendFront
    , testProvider "Extend end of signal"        R2.testExtendEnd
                                                 R2.dataExtendEnd
    , testProvider "Remove first & last element" R2.testTrim R2.dataTrim
  ],
  testGroup "C1" [
      testProvider "DWT"                         C1.testDwt  C1.dataDwt
    , testProvider "IDWT"                        C1.testIdwt C1.dataIdwt
    , testProperty "DWT-IDWT identity"           C1.propDWTInvertible
  ],
  testGroup "Verify equivalence of all DWT/IDWT implementations" [
      testProperty " DWT Eval1 like List1"       RF.propDWTEval1LikeList1
    , testProperty "IDWT Eval1 like List1"       RF.propIDWTEval1LikeList1
    , testProperty " DWT List2 like Eval1"       RF.propDWTList2LikeEval1
    , testProperty "IDWT List2 like Eval1"       RF.propIDWTList2LikeEval1
    , testProperty " DWT Eval2 like List2"       RF.propDWTEval2LikeList2
    , testProperty "IDWT Eval2 like List2"       RF.propIDWTEval2LikeList2
    , testProperty " DWT Repa1 like List1"       RF.propDWTRepa1LikeList1
    , testProperty "IDWT Repa1 like List1"       RF.propIDWTRepa1LikeList1
    , testProperty " DWT Repa2 like Repa1"       RF.propDWTRepa2LikeRepa1
    , testProperty "IDWT Repa2 like Repa1"       RF.propIDWTRepa2LikeRepa1
    , testProperty " DWT    C1 like List1"       RF.propDWTC1LikeList1
    , testProperty "IDWT    C1 like List1"       RF.propIDWTC1LikeList1
    , testProperty " DWT Vector1 like  C1"       RF.propDWTVector1LikeC1
    , testProperty "IDWT Vector1 like  C1"       RF.propIDWTVector1LikeC1
  ]
 ]
