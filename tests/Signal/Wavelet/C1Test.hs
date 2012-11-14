module Signal.Wavelet.C1Test where

import Data.Vector.Storable as V
import Signal.Wavelet.C1
import qualified Signal.Wavelet.List1 as L1
import qualified Signal.Wavelet.List.Common as LC
import Signal.Wavelet.Vector.Common
import Test.ArbitraryInstances
import Test.HUnit
import Test.Utils


testDwt :: (Vector Double, Vector Double, Vector Double) -> Assertion
testDwt (ls, sig, expected) = 
    expected @=~? dwt ls sig


dataDwt :: [(Vector Double, Vector Double, Vector Double)] 
dataDwt =
    [ (
       toRad $ fromList [30,25,40], 
       fromList [1,2,2,4,-3,5,0,1,1,-1,-2,2,4,5,6,3], 
       fromList [ -0.869330716850108, -1.33074602494193, -4.452066284456580, 
                  -0.766339042879150, -3.99023927679201,  3.273575105871030, 
                  -2.639689358691720, -1.39229920071584,  0.062440000137053, 
                  -1.159888007129840,  0.97906335585356,  0.763494159561419, 
                  -4.563606712907260, -4.76673895168943, -4.662257981490680, 
                  -5.417080918602780 ] 
      ),
      ( fromList $ [], 
        fromList $ [1,2,2,4,-3,5,0,1,1,-1,-2,2,4,5,6,3],
        fromList $ [1,2,2,4,-3,5,0,1,1,-1,-2,2,4,5,6,3] 
      ),
      ( fromList $ [],
        fromList $ [],
        fromList $ []
      ),
      ( fromList $ [1,2,3],
        fromList $ [],
        fromList $ []
      )
    ]


testIdwt :: (Vector Double, Vector Double, Vector Double) -> Assertion
testIdwt (ls, sig, expected) = 
    expected @=~? idwt ls sig


dataIdwt :: [(Vector Double, Vector Double, Vector Double)] 
dataIdwt =
    [ (
       toRad $ fromList [40,25,30], 
       fromList [ -0.869330716850108, -1.33074602494193, -4.452066284456580, 
                  -0.766339042879150, -3.99023927679201,  3.273575105871030, 
                  -2.639689358691720, -1.39229920071584,  0.062440000137053, 
                  -1.159888007129840,  0.97906335585356,  0.763494159561419, 
                  -4.563606712907260, -4.76673895168943, -4.662257981490680, 
                  -5.417080918602780 ],
       fromList [1,2,2,4,-3,5,0,1,1,-1,-2,2,4,5,6,3] 
      ),
      ( fromList $ [], 
        fromList $ [1,2,2,4,-3,5,0,1,1,-1,-2,2,4,5,6,3],
        fromList $ [1,2,2,4,-3,5,0,1,1,-1,-2,2,4,5,6,3] 
      ),
      ( fromList $ [],
        fromList $ [],
        fromList $ []
      ),
      ( fromList $ [1,2,3],
        fromList $ [],
        fromList $ []
      )
    ]


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