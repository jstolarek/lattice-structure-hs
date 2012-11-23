module Signal.Wavelet.C1Test where

import Control.Arrow        ((&&&))
import Data.Vector.Storable (Vector, fromList, (!))
import Test.HUnit           (Assertion)
import Test.QuickCheck      (Property, forAll, elements)

import Signal.Wavelet.C1
import Signal.Wavelet.List.Common as LC (cslN, csrN)
import Signal.Wavelet.Vector.Common     (inv)
import Test.ArbitraryInstances          (DwtInputC(..))
import Test.Data.Wavelet          as DW
import Test.Utils                       ((=~), (@=~?))


testDwt :: (Vector Double, Vector Double, Vector Double) -> Assertion
testDwt (ls, sig, expected) = 
    expected @=~? dwt ls sig


dataDwt :: [(Vector Double, Vector Double, Vector Double)] 
dataDwt = Prelude.map f DW.dataDwt
    where f (a, b, c) = (fromList a, fromList b, g c)
              where g xs = fromList . LC.csrN n $ xs
                    n    = max (Prelude.length a - 1) 0


testIdwt :: (Vector Double, Vector Double, Vector Double) -> Assertion
testIdwt (ls, sig, expected) = 
    expected @=~? idwt ls sig


dataIdwt :: [(Vector Double, Vector Double, Vector Double)] 
dataIdwt = Prelude.map f DW.dataIdwt
    where f (a, b, c) = (fromList a, fromList b, g c)
              where g xs = fromList . LC.cslN n $ xs
                    n    = max (Prelude.length a - 1) 0


propDWTInvertible :: DwtInputC -> Bool
propDWTInvertible (DwtInputC (ls, sig)) = 
    idwt (inv ls) (dwt ls sig) =~ sig


testLattice :: (Int, (Double, Double), Vector Double, Vector Double) 
            -> Assertion
testLattice (lm, baseOp, sig, expected) = 
    expected @=~? lattice lm baseOp sig


dataLattice :: [(Int, (Double, Double), Vector Double, Vector Double)]
dataLattice = Prelude.map (\(a, b, c, d) -> (a, b, fromList c, fromList d)) 
                DW.dataLatticeWithLM


propDoubleLatticeIdentity :: DwtInputC -> Property
propDoubleLatticeIdentity (DwtInputC (ls, sig)) =
    forAll (elements [0,1]) $ \lm ->
        lattice lm baseOp (lattice lm baseOp sig) =~ sig
            where
              baseOp = (sin &&& cos) $ ls ! 0
