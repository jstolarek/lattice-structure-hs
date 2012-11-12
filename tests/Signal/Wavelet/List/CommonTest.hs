module Signal.Wavelet.List.CommonTest where

import Signal.Wavelet.List.Common
import Test.HUnit
import Test.QuickCheck
import Test.Utils


testCsl :: ([Double], [Double]) -> Assertion
testCsl (input, expected) = 
    expected @=? csl input


dataCsl :: [([Double], [Double])]
dataCsl = 
    [
     ( [1,2,3,4], [2,3,4,1] ),
     ( [], [] )
    ]


testCsr :: ([Double], [Double]) -> Assertion
testCsr (input, expected) = 
    expected @=? csr input


dataCsr :: [([Double], [Double])]
dataCsr = 
    [
     ( [1,2,3,4], [4,1,2,3] ),
     ( [], [] )
    ]


testCslN :: (Int, [Double], [Double]) -> Assertion
testCslN (n, input, expected) = 
    expected @=? cslN n input


dataCslN :: [(Int, [Double], [Double])]
dataCslN = 
    [
     ( 0, [1,2,3,4], [1,2,3,4] ),
     ( 4, [], [] )
    ]


testCsrN :: (Int, [Double], [Double]) -> Assertion
testCsrN (n, input, expected) = 
    expected @=? csrN n input


dataCsrN :: [(Int, [Double], [Double])]
dataCsrN = 
    [
     ( 0, [1,2,3,4], [1,2,3,4] ),
     ( 4, [], [] )
    ]


propIdentityShift1 :: [Double] -> Bool
propIdentityShift1 xs = csl (csr xs) == xs


propIdentityShift2 :: [Double] -> Bool
propIdentityShift2 xs = csr (csl xs) == xs


propIdentityShift3 :: [Double] -> Property
propIdentityShift3 xs = forAll (sized $ \s -> 
    choose (1, s) ) $ \n -> 
        cslN n (csrN n xs) == xs


propIdentityShift4 :: [Double] -> Property
propIdentityShift4 xs = forAll (sized $ \s -> 
    choose (1, s)) $ \n -> 
        csrN n (cslN n xs) == xs


propIdentityShift5 :: [Double] -> Bool
propIdentityShift5 xs = cslN n xs == xs
    where n = length xs


propIdentityShift6 :: [Double] -> Bool
propIdentityShift6 xs = csrN n xs == xs
    where n = length xs


propLatticeInverseInverse :: [Double] -> Bool
propLatticeInverseInverse xs = inv (inv xs) == xs


propDegRadInvertible :: [Double] -> Bool
propDegRadInvertible xs = toDeg (toRad xs) =~ xs


propRadDegInvertible :: [Double] -> Bool
propRadDegInvertible xs = toRad (toDeg xs) =~ xs
