module Signal.Wavelet.List.CommonTest where

import Control.Arrow ((&&&))
import Signal.Wavelet.List.Common
import Test.ArbitraryInstances
import Test.HUnit
import Test.QuickCheck
import Test.Utils


testLattice :: ((Double, Double), [Double], [Double]) -> Assertion
testLattice (baseOp, sig, expected) = 
    expected @=~? latticeSeq baseOp sig


dataLattice :: [((Double, Double), [Double], [Double])]
dataLattice =
    [
      (
       (0.5, 0.8660254038), 
       [ 1, 2, 2, 4,-3, 5, 0, 1, 1,-1,-2, 2, 4, 5, 6, 3 ],
       [ 1.8660254038, -1.2320508076,  3.7320508076, -2.4641016151,
        -0.0980762114, -5.8301270189,  0.5000000000, -0.8660254038,
         0.3660254038,  1.3660254038, -0.7320508076, -2.7320508076,
         5.9641016151, -2.3301270189,  6.6961524227,  0.4019237886 ]
      ), (
       (0.5, 0.8660254038), 
       [], 
       []
      )
    ]


propDoubleLatticeIdentity :: DwtInputList -> Bool
propDoubleLatticeIdentity (DwtInputList (ls, sig)) =
    latticeSeq baseOp (latticeSeq baseOp sig) =~ sig
        where
          baseOp = (sin &&& cos) $ head ls


testExtendFront :: ([Double], Int, [Double]) -> Assertion
testExtendFront (sig, ln, expected) = 
    expected @=~? extendFront ln sig


dataExtendFront :: [([Double], Int, [Double])]
dataExtendFront =
   [
     ( [1,2,2,4,-3,5,0,1,1,-1,-2,2], 
       3,
       [1,-1,-2,2,1,2,2,4,-3,5,0,1,1,-1,-2,2]
     ),
     ( [1,2,3,4], 
       6,
       [3,4,1,2,3,4,1,2,3,4,1,2,3,4]
     ),
     ( [1,2], 
       3,
       [1,2,1,2,1,2]
     ),
     ( [1,2], 
       1,
       [1,2]
     ),
     ( [], 
       7,
       []
     )
   ]


testExtendEnd :: ([Double], Int, [Double]) -> Assertion
testExtendEnd (sig, ln, expected) = 
    expected @=~? extendEnd ln sig


dataExtendEnd :: [([Double], Int, [Double])]
dataExtendEnd =
   [
     ( [1,2,2,4,-3,5,0,1,1,-1,-2,2], 
       3,
       [1,2,2,4,-3,5,0,1,1,-1,-2,2,1,2,2,4]
     ),
     ( [1,2], 
       3,
       [1,2,1,2,1,2]
     ),
     ( [1,2], 
       1,
       [1,2]
     ),
     ( [], 
       7,
       []
     )
   ]


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
