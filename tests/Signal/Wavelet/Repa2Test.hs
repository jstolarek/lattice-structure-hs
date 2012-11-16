module Signal.Wavelet.Repa2Test where

import Control.Arrow   ((&&&))
import Data.Array.Repa
import Test.HUnit      (Assertion)

import Signal.Wavelet.Repa2
import Signal.Wavelet.Repa.Common (inv)
import Test.ArbitraryInstances    (DwtInputRepa(..))
import Test.Data.Wavelet          as DW
import Test.Utils                 ((=~), (@=~?))


testDwt :: (Array U DIM1 Double, Array U DIM1 Double, Array U DIM1 Double)
        -> Assertion
testDwt (ls, sig, expected) = 
    expected @=~? dwt ls sig


dataDwt :: [(Array U DIM1 Double, Array U DIM1 Double, Array U DIM1 Double)]
dataDwt = Prelude.map (DW.all3 f) DW.dataDwt


testIdwt :: (Array U DIM1 Double, Array U DIM1 Double, Array U DIM1 Double)
        -> Assertion
testIdwt (ls, sig, expected) = 
    expected @=~? idwt ls sig


dataIdwt :: [(Array U DIM1 Double, Array U DIM1 Double, Array U DIM1 Double)]
dataIdwt = Prelude.map (DW.all3 f) DW.dataIdwt


propDWTInvertible :: DwtInputRepa -> Bool
propDWTInvertible (DwtInputRepa (ls, sig)) = 
    idwt (computeS $ inv ls) (dwt ls sig) =~ sig


testLattice :: ((Double, Double), Array U DIM1 Double, Array U DIM1 Double)
             -> Assertion
testLattice (baseOp, sig, expected) = 
    expected @=~? computeS (lattice baseOp sig)


dataLattice :: [((Double,Double), Array U DIM1 Double, Array U DIM1 Double)]
dataLattice = Prelude.map (\(a, b, c) -> (a, f b, f c)) DW.dataLattice


propDoubleLatticeIdentity :: DwtInputRepa -> Bool
propDoubleLatticeIdentity (DwtInputRepa (ls, sig)) =
    computeS (lattice baseOp (computeS $ lattice baseOp sig)) =~ sig
        where
          baseOp = (sin &&& cos) $ ls ! (Z :. 0)


testExtendFront :: (Int, Array U DIM1 Double, Array U DIM1 Double)
              -> Assertion
testExtendFront (ln, sig, expected) = 
    expected @=~? (computeS . extendFront ln $ sig)


dataExtendFront :: [(Int, Array U DIM1 Double, Array U DIM1 Double)]
dataExtendFront = Prelude.map (\(a, b, c) -> (a, f b, f c)) DW.dataExtendFront


testExtendEnd :: (Int, Array U DIM1 Double, Array U DIM1 Double)
              -> Assertion
testExtendEnd (ln, sig, expected) = 
    expected @=~? (computeS . extendEnd ln $ sig)


dataExtendEnd :: [(Int, Array U DIM1 Double, Array U DIM1 Double)]
dataExtendEnd = Prelude.map (\(a, b, c) -> (a, f b, f c)) DW.dataExtendEnd


testTrim :: (Array U DIM1 Double, Array U DIM1 Double)
         -> Assertion
testTrim (sig, expected) = 
    expected @=~? (computeS . trim . delay $ sig)


dataTrim :: [(Array U DIM1 Double, Array U DIM1 Double)]
dataTrim = Prelude.map (DW.all2 f) DW.dataTrim


f :: [Double] -> Array U DIM1 Double
f xs = fromListUnboxed (Z :. (length xs)) xs
