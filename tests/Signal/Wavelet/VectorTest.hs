module Signal.Wavelet.VectorTest where

import Data.Vector.Unboxed as V
import qualified Signal.Wavelet.C as C
import Signal.Wavelet.Vector
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


propDWTInvertible :: DwtInputVector -> Bool
propDWTInvertible (DwtInputVector (ls, sig)) = 
    idwt (inv ls) (dwt ls sig) =~ sig


propDWTIdenticalToC :: DwtInputVector -> Bool
propDWTIdenticalToC (DwtInputVector (ls, sig)) = 
    cDwt =~ repaDwt
        where
          cDwt    = C.dwt (convert ls) (convert sig)
          repaDwt = convert $ dwt ls sig


propIDWTIdenticalToC :: DwtInputVector -> Bool
propIDWTIdenticalToC (DwtInputVector (ls, sig)) = 
    cIdwt =~ repaIdwt
        where
          cIdwt    = C.idwt (convert ls) (convert sig)
          repaIdwt = convert $ idwt ls sig
