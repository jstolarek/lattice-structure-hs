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
import qualified Signal.Wavelet.Repa1Test       as R


main :: IO ()
main = defaultMain tests


tests :: [Test]
tests = [ 
  testGroup "C" [
      testWithProvider "DWT"                     C.testDwt
                                                 C.dataDwt
    , testWithProvider "IDWT"                    C.testIdwt
                                                 C.dataIdwt
    , testProperty "DWT-IDWT identity"           C.propDWTInvertible
    , testProperty  "DWT identical to List imp." C.propDWTIdenticalToList
    , testProperty "IDWT identical to List imp." C.propIDWTIdenticalToList
  ],
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
  testGroup "Repa common" [
      testProperty "Deg-Rad identity"            RC.propDegRadInvertible
    , testProperty "Rad-Deg identity"            RC.propRadDegInvertible
  ],
  testGroup "Repa1" [
      testWithProvider "DWT"                     R.testDwt
                                                 R.dataDwt
    , testWithProvider "IDWT"                    R.testIdwt
                                                 R.dataIdwt
    , testProperty "DWT-IDWT identity"           R.propDWTInvertible
    , testWithProvider "Lattice layer"           R.testLattice 
                                                 R.dataLattice
    , testProperty "Inverting lattice identity"  R.propDoubleLatticeInverse
    , testWithProvider "Cyclic shift left"       R.testCsl
                                                 R.dataCsl
    , testWithProvider "Cyclic shift right"      R.testCsr
                                                 R.dataCsr
    , testWithProvider "Cyclic shift left by N"  R.testCslN
                                                 R.dataCslN
    , testWithProvider "Cyclic shift right by N" R.testCsrN
                                                 R.dataCsrN
    , testProperty "L/R shift composition"       R.propIdentityShift1
    , testProperty "R/L shift composition"       R.propIdentityShift2
    , testProperty "L/R shift by N composition"  R.propIdentityShift3
    , testProperty "R/L shift by N composition"  R.propIdentityShift4
    , testProperty "Periodic left shift"         R.propIdentityShift5
    , testProperty "Periodic right shift"        L.propIdentityShift6
    , testProperty "List-Pairs-List identity"    R.propPairsIdentity1
    , testProperty "Pairs-List-Pairs identity"   R.propPairsIdentity2
    ]
  ]
