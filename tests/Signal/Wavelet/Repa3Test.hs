module Signal.Wavelet.Repa3Test where

import Control.Arrow       ((&&&))
import Data.Array.Repa     hiding (map)
import Data.Vector.Unboxed (Vector, Unbox, fromList)
import Test.HUnit          (Assertion)

import Signal.Wavelet.Repa3
--import Signal.Wavelet.Repa.Common (inv)
import Test.ArbitraryInstances    (DwtInputRepa(..))
import Test.Data.Wavelet          as DW
import Test.Utils                 ((=~), (@=~?))

{-
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

-}
testLattice :: ((Double, Double), Array U DIM1 Double, Array U DIM1 Double)
             -> Assertion
testLattice (baseOp, sig, expected) = 
    expected @=~? lattice baseOp sig


dataLattice :: [((Double,Double), Array U DIM1 Double, Array U DIM1 Double)]
dataLattice = map (\(a, b, c) -> (a, f b, f c)) DW.dataLattice


propDoubleLatticeIdentity :: DwtInputRepa -> Bool
propDoubleLatticeIdentity (DwtInputRepa (ls, sig)) =
    (lattice baseOp (lattice baseOp sig)) =~ sig
        where
          baseOp = (sin &&& cos) $ ls ! (Z :. 0)


testDistributeWork :: (Int, Int, Vector Int) -> Assertion
testDistributeWork (threads, sigLength, expected) =
    expected @=~? distributeWork threads sigLength


dataDistributeWork :: [(Int, Int, Vector Int)]
dataDistributeWork = map (\(a, b, c) -> (a, b, g c)) DW.dataDistributeWork


f :: [Double] -> Array U DIM1 Double
f xs = fromListUnboxed (Z :. (length xs)) xs


g :: (Unbox a) => [a] -> Vector a
g xs =fromList xs
