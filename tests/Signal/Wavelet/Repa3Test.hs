module Signal.Wavelet.Repa3Test where

import Control.Arrow              ((&&&))
import Data.Array.Repa            hiding (map)
import Test.HUnit                 (Assertion)
import Test.QuickCheck            (Property, forAll, elements)

import Signal.Wavelet.List.Common as LC (cslN, csrN)
import Signal.Wavelet.Repa3
import Signal.Wavelet.Repa.Common (forceS, forceP, inv)
import Test.ArbitraryInstances    (DwtInputRepa(..))
import Test.Data.Wavelet          as DW
import Test.Utils                 ((=~), (@=~?))


testDwt :: (Array U DIM1 Double, Array U DIM1 Double, Array U DIM1 Double)
        -> Assertion
testDwt (ls, sig, expected) = 
    expected @=~? dwtS ls sig


dataDwt :: [(Array U DIM1 Double, Array U DIM1 Double, Array U DIM1 Double)]
dataDwt = Prelude.map convertTuple DW.dataDwt
    where convertTuple (a, b, c) = (f a, f b, g c)
              where g = f . LC.csrN n
                    n = max (Prelude.length a - 1) 0


testIdwt :: (Array U DIM1 Double, Array U DIM1 Double, Array U DIM1 Double)
        -> Assertion
testIdwt (ls, sig, expected) = 
    expected @=~? idwtS ls sig


dataIdwt :: [(Array U DIM1 Double, Array U DIM1 Double, Array U DIM1 Double)]
dataIdwt = Prelude.map convertTuple DW.dataIdwt
    where convertTuple (a, b, c) = (f a, f b, g c)
              where g = f . LC.cslN n
                    n = max (Prelude.length a - 1) 0


propDWTInvertible :: DwtInputRepa -> Bool
propDWTInvertible (DwtInputRepa (ls, sig)) = 
    idwtS (computeS $ inv ls) (dwtS ls sig) =~ sig


testLatticeS :: (Int, (Double,Double), Array U DIM1 Double, Array U DIM1 Double)
             -> Assertion
testLatticeS (lm, baseOp, sig, expected) = 
    expected @=~? forceS (lattice lm baseOp sig)


testLatticeP :: (Int, (Double,Double), Array U DIM1 Double, Array U DIM1 Double)
             -> Assertion
testLatticeP (lm, baseOp, sig, expected) = 
    expected @=~? forceP (lattice lm baseOp sig)


dataLattice ::[(Int, (Double,Double), Array U DIM1 Double, Array U DIM1 Double)]
dataLattice = map (\(a, b, c, d) -> (a, b, f c, f d)) DW.dataLatticeWithLM


propDoubleLatticeSIdentity :: DwtInputRepa -> Property
propDoubleLatticeSIdentity (DwtInputRepa (ls, sig)) =
    forAll (elements [0,1]) $ \lm ->
        (forceS (lattice lm baseOp (forceS $ lattice lm baseOp sig))) =~ sig
            where baseOp   = (sin &&& cos) $ ls ! (Z :. 0)


propDoubleLatticePIdentity :: DwtInputRepa -> Property
propDoubleLatticePIdentity (DwtInputRepa (ls, sig)) =
    forAll (elements [0,1]) $ \lm ->
        (forceP (lattice lm baseOp (forceP $ lattice lm baseOp sig))) =~ sig
            where baseOp   = (sin &&& cos) $ ls ! (Z :. 0)


f :: [Double] -> Array U DIM1 Double
f xs = fromListUnboxed (Z :. (length xs)) xs
