module Signal.Wavelet.List.CommonTest where

import Control.Arrow ((&&&))
import Signal.Wavelet.List.Common
import Test.ArbitraryInstances    (DwtInputList(..))
import qualified Test.Data.Wavelet as DW
import Test.HUnit                 (Assertion, (@=?))
import Test.QuickCheck
import Test.Utils                 ((=~), (@=~?))


testLattice :: ((Double, Double), [Double], [Double]) -> Assertion
testLattice (baseOp, sig, expected) = 
    expected @=~? latticeSeq baseOp sig


dataLattice :: [((Double, Double), [Double], [Double])]
dataLattice = DW.dataLattice


propDoubleLatticeIdentity :: DwtInputList -> Bool
propDoubleLatticeIdentity (DwtInputList (ls, sig)) =
    latticeSeq baseOp (latticeSeq baseOp sig) =~ sig
        where
          baseOp = (sin &&& cos) $ head ls


testExtendFront :: (Int, [Double], [Double]) -> Assertion
testExtendFront (ln, sig, expected) = 
    expected @=~? extendFront ln sig


dataExtendFront :: [(Int, [Double], [Double])]
dataExtendFront = DW.dataExtendFront


testExtendEnd :: (Int, [Double], [Double]) -> Assertion
testExtendEnd (ln, sig, expected) = 
    expected @=~? extendEnd ln sig


dataExtendEnd :: [(Int, [Double], [Double])]
dataExtendEnd = DW.dataExtendEnd


testCsl :: ([Double], [Double]) -> Assertion
testCsl (input, expected) = 
    expected @=? csl input


dataCsl :: [([Double], [Double])]
dataCsl = DW.dataCsl


testCsr :: ([Double], [Double]) -> Assertion
testCsr (input, expected) = 
    expected @=? csr input


dataCsr :: [([Double], [Double])]
dataCsr = DW.dataCsr


testCslN :: (Int, [Double], [Double]) -> Assertion
testCslN (n, input, expected) = 
    expected @=? cslN n input


dataCslN :: [(Int, [Double], [Double])]
dataCslN = DW.dataCslN


testCsrN :: (Int, [Double], [Double]) -> Assertion
testCsrN (n, input, expected) = 
    expected @=? csrN n input


dataCsrN :: [(Int, [Double], [Double])]
dataCsrN = DW.dataCsrN


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
