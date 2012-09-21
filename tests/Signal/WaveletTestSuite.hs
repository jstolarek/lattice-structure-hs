module Main ( main ) where

--import Distribution.TestSuite
import Test.Framework
import Test.Framework.Providers.QuickCheck2

import Signal.WaveletTest

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = 
  [ 
    testGroup "Signal shifts" 
    [ 
      testProperty "L/R shift composition" propIdentityShift1 ,
      testProperty "R/L shift composition" propIdentityShift2
    ]
  ]
