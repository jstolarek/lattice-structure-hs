module Main ( main ) where

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit
import qualified Test.HUnit as HU

import Signal.WaveletTest

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = 
  [ 
    testGroup "Signal shifts" 
    [ 
      testProperty "L/R shift composition"  propIdentityShift1 ,
      testProperty "R/L shift composition"  propIdentityShift2 ,
      testWithProvider "Cyclic shift left"  testCyclicShiftLeft 
                                            testCyclicShiftLeftDataProvider,
      testWithProvider "Cyclic shift right" testCyclicShiftRight
                                            testCyclicShiftRightDataProvider
    ]
  ]

testWithProvider :: String -> (a -> HU.Assertion) -> [a] -> Test
testWithProvider testGroupName testFunction =
    testGroup testGroupName . map createTest . zipWith assignName [1 :: Int ..]
      where 
        createTest (name, dataSet)   = testCase name $ testFunction dataSet
        assignName setNumber dataSet = ("Data set " ++ show setNumber, dataSet)
