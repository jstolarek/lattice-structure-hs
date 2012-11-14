module Main ( 
    main 
 ) where

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Utils

import qualified Signal.Wavelet.C1Test          as C1
import qualified Signal.Wavelet.Eval1Test       as E1
import qualified Signal.Wavelet.List.CommonTest as LC
import qualified Signal.Wavelet.List1Test       as L1
import qualified Signal.Wavelet.List2Test       as L2
import qualified Signal.Wavelet.Repa.CommonTest as RC
import qualified Signal.Wavelet.Repa1Test       as R1
import qualified Signal.Wavelet.Repa2Test       as R2
import qualified Signal.Wavelet.Vector1Test     as V1


main :: IO ()
main = defaultMain tests


tests :: [Test]
tests = [ 
   testGroup "Lists common" [
      testProvider "Lattice layer"               LC.testLattice 
                                                 LC.dataLattice
    , testProperty "Inverting lattice layer"     LC.propDoubleLatticeIdentity
    , testProperty "Inverting lattice structure" LC.propLatticeInverseInverse
    , testProperty "Deg-Rad identity"            LC.propDegRadInvertible
    , testProperty "Rad-Deg identity"            LC.propRadDegInvertible
    , testProvider "Cyclic shift left"           LC.testCsl
                                                 LC.dataCsl
    , testProvider "Cyclic shift right"          LC.testCsr
                                                 LC.dataCsr
    , testProvider "Cyclic shift left by N"      LC.testCslN
                                                 LC.dataCslN
    , testProvider "Cyclic shift right by N"     LC.testCsrN
                                                 LC.dataCsrN
    , testProperty "L/R shift composition"       LC.propIdentityShift1
    , testProperty "R/L shift composition"       LC.propIdentityShift2
    , testProperty "L/R shift by N composition"  LC.propIdentityShift3
    , testProperty "R/L shift by N composition"  LC.propIdentityShift4
    , testProperty "Periodic left shift"         LC.propIdentityShift5
    , testProperty "Periodic right shift"        LC.propIdentityShift6
  ],
  testGroup "Lists" [ 
      testProvider "DWT"                         L1.testDwt
                                                 L1.dataDwt
    , testProvider "IDWT"                        L1.testIdwt
                                                 L1.dataIdwt
    , testProperty "DWT-IDWT identity"           L1.propDWTInvertible
  ],
  testGroup "Lists" [ 
      testProvider "DWT"                         L2.testDwt
                                                 L2.dataDwt
    , testProvider "IDWT"                        L2.testIdwt
                                                 L2.dataIdwt
    , testProperty "DWT-IDWT identity"           L2.propDWTInvertible
    , testProperty " DWT like List1 implement."  L2.propDWTIdenticalToList1
    , testProperty "IDWT like List1 implement."  L2.propIDWTIdenticalToList1
    , testProvider "Extend front of signal"      L2.testExtendFront
                                                 L2.dataExtendFront
    , testProvider "Extend end of signal"        L2.testExtendEnd
                                                 L2.dataExtendEnd
  ],
  testGroup "Eval" [ 
      testProvider "DWT"                         E1.testDwt
                                                 E1.dataDwt
    , testProvider "IDWT"                        E1.testIdwt
                                                 E1.dataIdwt
    , testProperty "DWT-IDWT identity"           E1.propDWTInvertible
    , testProperty "Lattice like List implemnt." E1.propLatticeIdenticalToList
  ],
  testGroup "Vector" [
      testProvider "DWT"                         V1.testDwt
                                                 V1.dataDwt
    , testProvider "IDWT"                        V1.testIdwt
                                                 V1.dataIdwt
    , testProperty "DWT-IDWT identity"           V1.propDWTInvertible
    , testProvider "Lattice layer"               V1.testLattice 
                                                 V1.dataLattice
    , testProperty "Inverting lattice layer"     V1.propDoubleLatticeIdentity
    , testProperty  "DWT like C implementation"  V1.propDWTIdenticalToC
    , testProperty "IDWT like C implementation"  V1.propIDWTIdenticalToC
  ],
  testGroup "Repa common" [
      testProperty "Deg-Rad identity"            RC.propDegRadInvertible
    , testProperty "Rad-Deg identity"            RC.propRadDegInvertible
    , testProperty "Inverting lattice structure" RC.propLatticeInverseInverse
  ],
  testGroup "Repa1" [
      testProvider "DWT"                         R1.testDwt
                                                 R1.dataDwt
    , testProvider "IDWT"                        R1.testIdwt
                                                 R1.dataIdwt
    , testProperty "DWT-IDWT identity"           R1.propDWTInvertible
    , testProvider "Lattice layer"               R1.testLattice 
                                                 R1.dataLattice
    , testProperty "Inverting lattice layer"     R1.propDoubleLatticeIdentity
    , testProperty " DWT like Vector implement." R1.propDWTIdenticalToVector
    , testProperty "IDWT like Vector implement." R1.propIDWTIdenticalToVector
    , testProvider "Cyclic shift left"           R1.testCsl
                                                 R1.dataCsl
    , testProvider "Cyclic shift right"          R1.testCsr
                                                 R1.dataCsr
    , testProvider "Cyclic shift left by N"      R1.testCslN
                                                 R1.dataCslN
    , testProvider "Cyclic shift right by N"     R1.testCsrN
                                                 R1.dataCsrN
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
      testProvider "DWT"                         R2.testDwt
                                                 R2.dataDwt
    , testProvider "IDWT"                        R2.testIdwt
                                                 R2.dataIdwt
    , testProperty "DWT-IDWT identity"           R2.propDWTInvertible
    , testProvider "Lattice layer"               R2.testLattice 
                                                 R2.dataLattice
    , testProperty "Inverting lattice layer"     R2.propDoubleLatticeIdentity
    , testProperty " DWT like Repa1 implement."  R2.propDWTIdenticalToRepa1
    , testProperty "IDWT like Repa1 implement."  R2.propIDWTIdenticalToRepa1
    , testProvider "Extend front of signal"      R2.testExtendFront
                                                 R2.dataExtendFront
    , testProvider "Extend end of signal"        R2.testExtendEnd
                                                 R2.dataExtendEnd
    , testProvider "Remove first & last element" R2.testTrim
                                                 R2.dataTrim
  ],
  testGroup "C" [
      testProvider "DWT"                         C1.testDwt
                                                 C1.dataDwt
    , testProvider "IDWT"                        C1.testIdwt
                                                 C1.dataIdwt
    , testProperty "DWT-IDWT identity"           C1.propDWTInvertible
    , testProperty " DWT like List implement."   C1.propDWTIdenticalToList
    , testProperty "IDWT like List implement."   C1.propIDWTIdenticalToList
  ]
 ]


{-

Note [Verifying accordance with reference implementation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

List implementation of DWT and IDWT is assumed to be a reference one. Each 
implementation verifies that it is identical to another one. The following 
dependencies are used:

List1 -> C1 -> Vector1 -> Repa1 -> Repa2

Read "List1 -> C1" as "List1 implementation serves as a reference to C1 
implementation".

-}
