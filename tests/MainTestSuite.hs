module Main ( 
    main 
 ) where

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Utils

import qualified Signal.WaveletTest as W
import qualified Signal.Repa.WaveletTest as R

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = 
  [ 
    testGroup "Signal shifts" 
    [ 
      testProperty "Inverting DWT"               W.propDWTInvertible
    , testWithProvider "DWT"                     W.testDwt
                                                 W.dataProviderDwt
    , testProperty "Inverting lattice structure" W.propDoubleLatticeInverse
    , testProperty "L/R shift composition"       W.propIdentityShift1
    , testProperty "R/L shift composition"       W.propIdentityShift2
    , testWithProvider "Cyclic shift left"       W.testCyclicShiftLeft
                                                 W.dataProviderCyclicShiftLeft
    , testWithProvider "Cyclic shift right"      W.testCyclicShiftRight
                                                 W.dataProviderCyclicShiftRight
    , testProperty "Deg-Rad identity"            W.propDegRadInvertible
    , testProperty "Rad-Deg identity"            W.propRadDegInvertible
  ],
    testGroup "Repa wavelets"
    [
      testProperty "List-Pairs-List identity"    R.propPairsIdentity1
    , testProperty "Pairs-List-Pairs identity"   R.propPairsIdentity2
    , testProperty "Inverting lattice structure" R.propDoubleLatticeInverse
    , testProperty "L/R shift composition"       R.propIdentityShift1
    , testProperty "R/L shift composition"       R.propIdentityShift2
    , testWithProvider "Cyclic shift left"       R.testCyclicShiftLeft
                                                 R.dataProviderCyclicShiftLeft
    , testWithProvider "Cyclic shift right"      R.testCyclicShiftRight
                                                 R.dataProviderCyclicShiftRight
    , testWithProvider "Lattice layer"           R.testLatticeLayer 
                                                 R.dataLatticeLayer
    , testProperty "Deg-Rad identity"            R.propDegRadInvertible
    , testProperty "Rad-Deg identity"            R.propRadDegInvertible
    ]
  ]
