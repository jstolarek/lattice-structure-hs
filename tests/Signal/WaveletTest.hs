module Signal.WaveletTest where

import Signal.Wavelet
import Test.HUnit
import Test.QuickCheck
import Test.Utils

-- DWT & IDWT

propDWTInvertible :: LS -> [Double] -> Property
propDWTInvertible ls sig = 
    (not . odd . length $ sig) ==>
        idwt (invLS ls) (dwt ls sig) =~ sig

testDwt :: (LS, [Double], [Double]) -> Assertion
testDwt (ls, sig, expected) = 
    expected @=~? dwt ls sig

dataProviderDwt :: [(LS, [Double], [Double])] 
dataProviderDwt =
    [
      (toRad [30,25,40], [1,2,2,4,-3,5,0,1,1,-1,-2,2,4,5,6,3], 
      [ -4.4520662844565800, -0.766339042879150, -3.990239276792010,  
         3.2735751058710300, -2.639689358691720, -1.392299200715840,
         0.0624400001370536, -1.159888007129840,  0.979063355853563,  
         0.7634941595614190, -4.563606712907260, -4.766738951689430, 
        -4.6622579814906800, -5.417080918602780, -0.869330716850108, 
        -1.3307460249419300
       ] )
    ]

-- inverting LS

propDoubleLatticeInverse :: LS -> Bool
propDoubleLatticeInverse xs = invLS (invLS xs) == xs

-- cyclic shift left & right tests

propIdentityShift1 :: [Double] -> Bool
propIdentityShift1 xs = cyclicShiftLeft (cyclicShiftRight xs) == xs

propIdentityShift2 :: [Double] -> Bool
propIdentityShift2 xs = cyclicShiftRight (cyclicShiftLeft xs) == xs

testCyclicShiftLeft :: ([Double], [Double]) -> Assertion
testCyclicShiftLeft (input, expected) = 
    expected @=? cyclicShiftLeft input

testCyclicShiftRight :: ([Double], [Double]) -> Assertion
testCyclicShiftRight (input, expected) = 
    expected @=? cyclicShiftRight input

dataProviderCyclicShiftLeft :: [([Double], [Double])]
dataProviderCyclicShiftLeft = 
    [
     ( [1,2,3,4], [2,3,4,1] )
    ]

dataProviderCyclicShiftRight :: [([Double], [Double])]
dataProviderCyclicShiftRight = 
    [
     ( [1,2,3,4], [4,1,2,3] )
    ]

-- toDeg & toRad
propDegRadInvertible :: [Double] -> Bool
propDegRadInvertible xs = toDeg (toRad xs) =~ xs

propRadDegInvertible :: [Double] -> Bool
propRadDegInvertible xs = toRad (toDeg xs) =~ xs
