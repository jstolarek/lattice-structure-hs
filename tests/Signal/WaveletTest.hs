module Signal.WaveletTest where

import Signal.Wavelet
import Test.HUnit

propIdentityShift1 :: [Double] -> Bool
propIdentityShift1 xs = cyclicShiftLeft (cyclicShiftRight xs) == id xs

propIdentityShift2 :: [Double] -> Bool
propIdentityShift2 xs = cyclicShiftRight (cyclicShiftLeft xs) == id xs

testCyclicShiftLeft :: ([Double], [Double]) -> Assertion
testCyclicShiftLeft (input, expected) = 
    expected @=? cyclicShiftLeft input

testCyclicShiftRight :: ([Double], [Double]) -> Assertion
testCyclicShiftRight (input, expected) = 
    expected @=? cyclicShiftRight input

testCyclicShiftLeftDataProvider :: [([Double], [Double])]
testCyclicShiftLeftDataProvider = 
    [
     ( [1,2,3,4], [2,3,4,1] )
    ]

testCyclicShiftRightDataProvider :: [([Double], [Double])]
testCyclicShiftRightDataProvider = 
    [
     ( [1,2,3,4], [4,1,2,3] )
    ]
