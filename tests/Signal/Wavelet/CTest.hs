module Signal.Wavelet.CTest where

import Data.Vector.Storable as V
import Signal.Wavelet.C
import qualified Signal.Wavelet.List  as L
import Test.ArbitraryInstances
import Test.HUnit
import Test.Utils


testDwt :: (Vector Double, Vector Double, Vector Double) -> Assertion
testDwt (ls, sig, expected) = 
    expected @=~? dwt ls sig


dataDwt :: [(Vector Double, Vector Double, Vector Double)] 
dataDwt =
    [ (toRad $ fromList [30,25,40], 
       fromList [1,2,2,4,-3,5,0,1,1,-1,-2,2,4,5,6,3], 
       fromList [ -0.869330716850108, -1.33074602494193, -4.452066284456580, 
                  -0.766339042879150, -3.99023927679201,  3.273575105871030, 
                  -2.639689358691720, -1.39229920071584,  0.062440000137053, 
                  -1.159888007129840,  0.97906335585356,  0.763494159561419, 
                  -4.563606712907260, -4.76673895168943, -4.662257981490680, 
                  -5.417080918602780
       ] )
    ]


testIdwt :: (Vector Double, Vector Double, Vector Double) -> Assertion
testIdwt (ls, sig, expected) = 
    expected @=~? idwt ls sig


dataIdwt :: [(Vector Double, Vector Double, Vector Double)] 
dataIdwt =
    [ (toRad $ fromList [40,25,30], 
       fromList [ -0.869330716850108, -1.33074602494193, -4.452066284456580, 
                  -0.766339042879150, -3.99023927679201,  3.273575105871030, 
                  -2.639689358691720, -1.39229920071584,  0.062440000137053, 
                  -1.159888007129840,  0.97906335585356,  0.763494159561419, 
                  -4.563606712907260, -4.76673895168943, -4.662257981490680, 
                  -5.417080918602780
       ],
       fromList [1,2,2,4,-3,5,0,1,1,-1,-2,2,4,5,6,3] )
    ]


propDWTInvertible :: DwtInputC -> Bool
propDWTInvertible (DwtInputC (ls, sig)) = 
    idwt (inv ls) (dwt ls sig) =~ sig


propDWTIdenticalToList :: DwtInputC -> Bool
propDWTIdenticalToList (DwtInputC (ls, sig)) = 
    listDwt =~ cDwt
        where
          listDwt = L.dwt (toList ls) (toList sig)
          cDwt    = shifts L.csl (V.length ls - 1) $ toList (dwt ls sig)


propIDWTIdenticalToList :: DwtInputC -> Bool
propIDWTIdenticalToList (DwtInputC (ls, sig)) = 
    listIdwt =~ cIdwt
        where
          listIdwt     = L.idwt (toList ls) (toList sig)
          cIdwt        = toList . idwt ls . shiftedInput ls $ sig
          shiftedInput xs ys = fromList (shifts L.csr (V.length xs - 1) (toList ys))


-- FIXME move somewhere else
shifts :: ([Double] -> [Double]) -> Int -> [Double] -> [Double]
shifts  _ 0 sig = sig
shifts cs n sig = shifts cs (n-1) (cs sig)
