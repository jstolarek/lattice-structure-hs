module Main ( 
    main 
 ) where

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Utils

import Signal.WaveletTest
import Signal.Repa.WaveletTest

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = 
  [ 
    testGroup "Signal shifts" 
    [ 
      testProperty "Inverting DWT"               propDWTInvertible
    , testWithProvider "DWT"                     testDwt
                                                 dataProviderDwt
    , testProperty "Inverting lattice structure" propDoubleLatticeInverse
    , testProperty "L/R shift composition"       propIdentityShift1
    , testProperty "R/L shift composition"       propIdentityShift2
    , testWithProvider "Cyclic shift left"       testCyclicShiftLeft
                                                 dataProviderCyclicShiftLeft
    , testWithProvider "Cyclic shift right"      testCyclicShiftRight
                                                 dataProviderCyclicShiftRight
    , testProperty "Deg-Rad identity"            propDegRadInvertible
    , testProperty "Rad-Deg identity"            propRadDegInvertible
    ], 
    testGroup "Repa wavelets"
    [
      testProperty "List-Pairs-List identity"    propPairsIdentity1
    , testProperty "Pairs-List-Pairs identity"   propPairsIdentity2
    ]
  ]
