module Signal.Wavelet.C1Test where

import Data.Vector.Storable as V
import Signal.Wavelet.C1
import qualified Signal.Wavelet.List1 as L1
import qualified Signal.Wavelet.List.Common as LC
import Signal.Wavelet.Vector.Common
import Test.ArbitraryInstances
import qualified Test.Data.Wavelet as DW
import Test.HUnit
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
                    n    = max (Prelude.length a - 1) 0


propDWTInvertible :: DwtInputC -> Bool
propDWTInvertible (DwtInputC (ls, sig)) = 
    idwt (inv ls) (dwt ls sig) =~ sig


propDWTIdenticalToList :: DwtInputC -> Bool
propDWTIdenticalToList (DwtInputC (ls, sig)) = 
    listDwt =~ cDwt
        where
          listDwt = L1.dwt (toList ls) (toList sig)
          cDwt    = LC.cslN (V.length ls - 1) $ toList (dwt ls sig)


propIDWTIdenticalToList :: DwtInputC -> Bool
propIDWTIdenticalToList (DwtInputC (ls, sig)) = 
    listIdwt =~ cIdwt
        where
          listIdwt         = L1.idwt (toList ls) (toList sig)
          cIdwt            = toList . idwt ls . shiftedSig ls $ sig
          shiftedSig xs ys = fromList (LC.csrN (V.length xs - 1) (toList ys))
