module Signal.Wavelet.Vector1Test where

import Control.Arrow ((&&&))
import Data.Vector.Unboxed as V
import qualified Signal.Wavelet.C1 as C1
import qualified Signal.Wavelet.List.Common as LC
import Signal.Wavelet.Vector1
import Signal.Wavelet.Vector.Common
import Test.ArbitraryInstances
import qualified Test.Data.Wavelet as DW
import Test.HUnit
import Test.QuickCheck
import Test.Utils


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
                    n = max (Prelude.length a - 1) 0


propDWTInvertible :: DwtInputVector -> Bool
propDWTInvertible (DwtInputVector (ls, sig)) = 
    idwt (inv ls) (dwt ls sig) =~ sig


testLattice :: (Int, (Double, Double), Vector Double, Vector Double) 
            -> Assertion
testLattice (lm, baseOp, sig, expected) = 
    expected @=~? lattice lm baseOp sig


dataLattice :: [(Int, (Double, Double), Vector Double, Vector Double)]
dataLattice = Prelude.map (\(a, b, c, d) -> (a, b, fromList c, fromList d)) 
                DW.dataLatticeWithLM


propDoubleLatticeIdentity :: DwtInputVector -> Property
propDoubleLatticeIdentity (DwtInputVector (ls, sig)) =
    forAll (elements [0,1]) $ \lm ->
        lattice lm baseOp (lattice lm baseOp sig) =~ sig
            where
              baseOp = (sin &&& cos) $ ls ! 0


propDWTIdenticalToC :: DwtInputVector -> Bool
propDWTIdenticalToC (DwtInputVector (ls, sig)) = 
    cDwt =~ repaDwt
        where
          cDwt    = C1.dwt (convert ls) (convert sig)
          repaDwt = convert $ dwt ls sig


propIDWTIdenticalToC :: DwtInputVector -> Bool
propIDWTIdenticalToC (DwtInputVector (ls, sig)) = 
    cIdwt =~ repaIdwt
        where
          cIdwt    = C1.idwt (convert ls) (convert sig)
          repaIdwt = convert $ idwt ls sig
