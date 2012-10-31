module Main ( 
    main 
 ) where

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Utils

import qualified Signal.Wavelet.CTest           as C
import qualified Signal.Wavelet.List.CommonTest as LC
import qualified Signal.Wavelet.ListTest        as L
import qualified Signal.Wavelet.Repa.CommonTest as RC
import qualified Signal.Wavelet.Repa1Test       as R1
import qualified Signal.Wavelet.Repa2Test       as R2
import qualified Signal.Wavelet.VectorTest      as V


main :: IO ()
main = defaultMain tests


tests :: [Test]
tests = [ 
   testGroup "Lists common" [
      testProperty "Deg-Rad identity"            LC.propDegRadInvertible
    , testProperty "Rad-Deg identity"            LC.propRadDegInvertible
  ],
  testGroup "Lists" [ 
      testWithProvider "DWT"                     L.testDwt
                                                 L.dataDwt
    , testWithProvider "IDWT"                    L.testIdwt
                                                 L.dataIdwt
    , testProperty "DWT-IDWT identity"           L.propDWTInvertible
    , testWithProvider "Lattice layer"           L.testLattice 
                                                 L.dataLattice
    , testProperty "Inverting lattice identity"  L.propDoubleLatticeInverse
    , testWithProvider "Cyclic shift left"       L.testCsl
                                                 L.dataCsl
    , testWithProvider "Cyclic shift right"      L.testCsr
                                                 L.dataCsr
    , testWithProvider "Cyclic shift left by N"  L.testCslN
                                                 L.dataCslN
    , testWithProvider "Cyclic shift right by N" L.testCsrN
                                                 L.dataCsrN
    , testProperty "L/R shift composition"       L.propIdentityShift1
    , testProperty "R/L shift composition"       L.propIdentityShift2
    , testProperty "L/R shift by N composition"  L.propIdentityShift3
    , testProperty "R/L shift by N composition"  L.propIdentityShift4
    , testProperty "Periodic left shift"         L.propIdentityShift5
    , testProperty "Periodic right shift"        L.propIdentityShift6
  ],
  testGroup "Vector" [
      testWithProvider "DWT"                     V.testDwt
                                                 V.dataDwt
    , testWithProvider "IDWT"                    V.testIdwt
                                                 V.dataIdwt
    , testProperty "DWT-IDWT identity"           V.propDWTInvertible
    , testProperty  "DWT identical to C impl."   V.propDWTIdenticalToC
    , testProperty "IDWT identical to C impl."   V.propIDWTIdenticalToC
  ],
  testGroup "Repa common" [
      testProperty "Deg-Rad identity"            RC.propDegRadInvertible
    , testProperty "Rad-Deg identity"            RC.propRadDegInvertible
    , testProperty "Inverting lattice identity"  RC.propDoubleLatticeInverse
  ],
  testGroup "Repa1" [
      testWithProvider "DWT"                     R1.testDwt
                                                 R1.dataDwt
    , testWithProvider "IDWT"                    R1.testIdwt
                                                 R1.dataIdwt
    , testProperty "DWT-IDWT identity"           R1.propDWTInvertible
    , testWithProvider "Lattice layer"           R1.testLattice 
                                                 R1.dataLattice
    , testWithProvider "Cyclic shift left"       R1.testCsl
                                                 R1.dataCsl
    , testWithProvider "Cyclic shift right"      R1.testCsr
                                                 R1.dataCsr
    , testWithProvider "Cyclic shift left by N"  R1.testCslN
                                                 R1.dataCslN
    , testWithProvider "Cyclic shift right by N" R1.testCsrN
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
      testWithProvider "DWT"                     R2.testDwt
                                                 R2.dataDwt
    , testWithProvider "IDWT"                    R2.testIdwt
                                                 R2.dataIdwt
    , testProperty "DWT-IDWT identity"           R2.propDWTInvertible
    , testProperty  "DWT identical to C impl."   R2.propDWTIdenticalToC
    , testProperty "IDWT identical to C impl."   R2.propIDWTIdenticalToC
    ], 
  testGroup "C" [
      testWithProvider "DWT"                     C.testDwt
                                                 C.dataDwt
    , testWithProvider "IDWT"                    C.testIdwt
                                                 C.dataIdwt
    , testProperty "DWT-IDWT identity"           C.propDWTInvertible
    , testProperty  "DWT identical to List imp." C.propDWTIdenticalToList
    , testProperty "IDWT identical to List imp." C.propIDWTIdenticalToList
   ]
  ]
