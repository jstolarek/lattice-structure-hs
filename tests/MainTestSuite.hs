module Main ( 
    main 
 ) where

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Utils

import qualified Signal.Wavelet.ListTest        as L
import qualified Signal.Wavelet.Repa.CommonTest as C
import qualified Signal.Wavelet.Repa1Test       as R


main :: IO ()
main = defaultMain tests


tests :: [Test]
tests = 
  [ 
  testGroup "Lists" 
    [ 
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
    , testProperty "L/R shift composition"       L.propIdentityShift1
    , testProperty "R/L shift composition"       L.propIdentityShift2
    , testProperty "Deg-Rad identity"            L.propDegRadInvertible
    , testProperty "Rad-Deg identity"            L.propRadDegInvertible
  ],
  testGroup "Repa common"
    [
      testProperty "Deg-Rad identity"            C.propDegRadInvertible
    , testProperty "Rad-Deg identity"            C.propRadDegInvertible
  ],
  testGroup "Repa1"
    [
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
    , testProperty "L/R shift composition"       R.propIdentityShift1
    , testProperty "R/L shift composition"       R.propIdentityShift2
    , testProperty "List-Pairs-List identity"    R.propPairsIdentity1
    , testProperty "Pairs-List-Pairs identity"   R.propPairsIdentity2
    ]
  ]
