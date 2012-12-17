module Signal.Wavelet.Repa3Test where

import Control.Arrow              ((&&&))
import Data.Array.Repa            hiding (map)
import qualified Data.Vector      as V (fromList, (!))
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


testLinearIndex :: (Array L DIM1 Double, Int, Double)
                -> Assertion
testLinearIndex (array, ind, expected) = 
    expected @=~? array `linearIndex` ind


dataLinearIndex :: [(Array L DIM1 Double, Int, Double)]
dataLinearIndex = Prelude.map transformData . filterEmpty $ DW.dataLatticeWithLM
    where filterEmpty = Prelude.filter (\(_,_,x,_) -> not . null $ x)
          transformData (lm, bOp, inL, outL) = 
              (ALattice (Z :. len) bOp (inV V.!) lm, ind, expected)
              where inV      = V.fromList inL
                    len      = length inL
                    ind     = len - 1
                    expected = (V.fromList outL) V.! ind


propLinearIndexSameAsLattice :: DwtInputRepa -> Int -> Property
propLinearIndexSameAsLattice (DwtInputRepa (ls, sig)) i =
    forAll (elements [0,1]) $ \lm ->
        (forceS . lattice lm baseOp $ sig) ! (Z :. ind) == 
                                               (array lm) `linearIndex` ind
            where baseOp = (sin &&& cos) $ ls ! (Z :. 0)
                  ind    = (abs i) `rem` len
                  len    = size . extent $ sig
                  array  = ALattice (Z :. len) baseOp ((sig !) . (Z :.))
    

f :: [Double] -> Array U DIM1 Double
f xs = fromListUnboxed (Z :. (length xs)) xs
