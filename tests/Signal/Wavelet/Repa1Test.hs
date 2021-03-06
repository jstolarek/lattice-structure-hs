module Signal.Wavelet.Repa1Test where

import Control.Arrow   ((&&&))
import Data.Array.Repa as R
import Test.HUnit      (Assertion, (@=?))
import Test.QuickCheck (Property, (==>))

import Signal.Wavelet.Repa1
import Signal.Wavelet.Repa.Common (forceS, forceP, inv)
import Test.ArbitraryInstances    (DwtInputRepa(..), RepaDIM1Array (..))
import Test.Data.Wavelet          as DW
import Test.Utils                 ((=~), (@=~?))


testDwt :: (Array U DIM1 Double, Array U DIM1 Double, Array U DIM1 Double)
        -> Assertion
testDwt (ls, sig, expected) =
    expected @=~? dwtS ls sig


dataDwt :: [(Array U DIM1 Double, Array U DIM1 Double, Array U DIM1 Double)]
dataDwt = Prelude.map (DW.all3 f) DW.dataDwt


testIdwt :: (Array U DIM1 Double, Array U DIM1 Double, Array U DIM1 Double)
        -> Assertion
testIdwt (ls, sig, expected) =
    expected @=~? idwtS ls sig


dataIdwt :: [(Array U DIM1 Double, Array U DIM1 Double, Array U DIM1 Double)]
dataIdwt = Prelude.map (DW.all3 f) DW.dataIdwt


propDWTInvertible :: DwtInputRepa -> Bool
propDWTInvertible (DwtInputRepa (ls, sig)) =
    idwtS (forceS $ inv ls) (dwtS ls sig) =~ sig


testLattice :: ((Double, Double), Array U DIM1 Double, Array U DIM1 Double)
            -> Assertion
testLattice (baseOp, sig, expected) =
    expected @=~? forceS (lattice baseOp sig)


dataLattice :: [((Double, Double), Array U DIM1 Double, Array U DIM1 Double)]
dataLattice = Prelude.map (\(a, b, c) -> (a, f b, f c)) DW.dataLattice


propDoubleLatticeIdentity :: DwtInputRepa -> Bool
propDoubleLatticeIdentity (DwtInputRepa (ls, sig)) =
    forceS (lattice baseOp (forceS $ lattice baseOp sig)) =~ sig
        where
          baseOp = (sin &&& cos) $ ls ! (Z :. 0)


testCsl :: (Array U DIM1 Double, Array U DIM1 Double) -> Assertion
testCsl (input, expected) =
    expected @=? (forceS $ csl input)


testCslPS :: (Array U DIM1 Double, Array U DIM1 Double) -> Assertion
testCslPS (input, expected) =
    expected @=? (forceS $ cslP input)


testCslPP :: (Array U DIM1 Double, Array U DIM1 Double) -> Assertion
testCslPP (input, expected) =
    expected @=? (forceP $ cslP input)


dataCsl :: [(Array U DIM1 Double, Array U DIM1 Double)]
dataCsl = Prelude.map (DW.all2 f) DW.dataCsl


testCsr :: (Array U DIM1 Double, Array U DIM1 Double) -> Assertion
testCsr (input, expected) =
    expected @=? (forceS $ csr input)


testCsrPS :: (Array U DIM1 Double, Array U DIM1 Double) -> Assertion
testCsrPS (input, expected) =
    expected @=? (forceS $ csrP input)


testCsrPP :: (Array U DIM1 Double, Array U DIM1 Double) -> Assertion
testCsrPP (input, expected) =
    expected @=? (forceP $ csrP input)


dataCsr :: [(Array U DIM1 Double, Array U DIM1 Double)]
dataCsr = Prelude.map (DW.all2 f) DW.dataCsr


testCslN :: (Int, Array U DIM1 Double, Array U DIM1 Double) -> Assertion
testCslN (n, input, expected) =
    expected @=? (forceS $ cslN n input)


dataCslN :: [(Int, Array U DIM1 Double, Array U DIM1 Double)]
dataCslN = Prelude.map (\(a, b, c) -> (a, f b, f c)) DW.dataCslN


testCsrN :: (Int, Array U DIM1 Double, Array U DIM1 Double) -> Assertion
testCsrN (n, input, expected) =
    expected @=? (forceS $ csrN n input)


dataCsrN :: [(Int, Array U DIM1 Double, Array U DIM1 Double)]
dataCsrN = Prelude.map (\(a, b, c) -> (a, f b, f c)) DW.dataCsrN


propIdentityShift1 :: RepaDIM1Array -> Bool
propIdentityShift1 (RepaDIM1Array xs) =
    forceS (csl . forceS . csr $ xs) == xs


propIdentityShift1P :: RepaDIM1Array -> Bool
propIdentityShift1P (RepaDIM1Array xs) =
    forceP (cslP . forceP . csrP $ xs) == xs


propIdentityShift2 :: RepaDIM1Array -> Bool
propIdentityShift2 (RepaDIM1Array xs) =
    forceS (csr . forceS . csl $ xs) == xs


propIdentityShift2P :: RepaDIM1Array -> Bool
propIdentityShift2P (RepaDIM1Array xs) =
    forceP (csrP . forceS . cslP $ xs) == xs


propIdentityShift3 :: Int -> RepaDIM1Array -> Bool
propIdentityShift3 n (RepaDIM1Array xs) =
    forceS (cslN n . forceS . csrN n $ xs) == xs


propIdentityShift4 :: Int -> RepaDIM1Array -> Bool
propIdentityShift4 n (RepaDIM1Array xs) =
    forceS (csrN n . forceS . cslN n $ xs) == xs


propIdentityShift5 :: RepaDIM1Array -> Bool
propIdentityShift5 (RepaDIM1Array xs) =
    forceS (cslN n xs) == xs
        where n = size . extent $ xs


propIdentityShift6 :: RepaDIM1Array -> Bool
propIdentityShift6 (RepaDIM1Array xs) =
    forceS (csrN n xs) == xs
        where n = size . extent $ xs


propPairsIdentity1 :: RepaDIM1Array -> Property
propPairsIdentity1 (RepaDIM1Array xs) =
    (even . size . extent $ xs) ==>
        forceS (fromPairs . toPairs $ xs) == xs


propPairsIdentity2 :: RepaDIM1Array -> Bool
propPairsIdentity2 (RepaDIM1Array xs) =
    forceS (toPairs . fromPairs $ ys) == ys
        where
          ys :: (Source U (Double, Double)) => Array U DIM1 (Double,Double)
          ys = forceS $ R.zipWith (,) xs xs


f :: [Double] -> Array U DIM1 Double
f xs = fromListUnboxed (Z :. (length xs)) xs
