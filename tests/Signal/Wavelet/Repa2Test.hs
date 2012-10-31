module Signal.Wavelet.Repa2Test where

import Data.Array.Repa as R
import qualified Data.Vector.Storable as V
import qualified Signal.Wavelet.C as C
import Signal.Wavelet.Repa.Common
import Signal.Wavelet.Repa2
import Test.ArbitraryInstances
import Test.HUnit
import Test.Utils


testDwt :: (Array U DIM1 Double, Array U DIM1 Double, Array U DIM1 Double)
        -> Assertion
testDwt (ls, sig, expected) = 
    expected @=~? dwt ls sig


dataDwt :: [(Array U DIM1 Double, Array U DIM1 Double, Array U DIM1 Double)]
dataDwt =
   [
     ( computeS . toRad $ fromListUnboxed (Z :. (3::Int))  [30,25,40], 
       fromListUnboxed (Z :. (16::Int)) $ [1,2,2,4,-3,5,0,1,1,-1,-2,2,4,5,6,3], 
       fromListUnboxed (Z :. (16::Int)) 
       [ -0.869330716850108, -1.33074602494193, -4.452066284456580, 
         -0.766339042879150, -3.99023927679201,  3.273575105871030, 
         -2.639689358691720, -1.39229920071584,  0.062440000137053, 
         -1.159888007129840,  0.97906335585356,  0.763494159561419, 
         -4.563606712907260, -4.76673895168943, -4.662257981490680, 
         -5.417080918602780 ] 
     ),
     ( fromListUnboxed (Z :. ( 0::Int)) $ []
     , fromListUnboxed (Z :. (16::Int)) $ [1.0,2,2,4,-3,5,0,1,1,-1,-2,2,4,5,6,3]
     , fromListUnboxed (Z :. (16::Int)) $ [1.0,2,2,4,-3,5,0,1,1,-1,-2,2,4,5,6,3]
     ),
     ( fromListUnboxed (Z :. (0::Int)) $ []
     , fromListUnboxed (Z :. (0::Int)) $ []
     , fromListUnboxed (Z :. (0::Int)) $ []
     ),
     ( fromListUnboxed (Z :. (3::Int)) $ [1,2,3]
     , fromListUnboxed (Z :. (0::Int)) $ []
     , fromListUnboxed (Z :. (0::Int)) $ []
     )
   ]


testIdwt :: (Array U DIM1 Double, Array U DIM1 Double, Array U DIM1 Double)
        -> Assertion
testIdwt (ls, sig, expected) = 
    expected @=~? idwt ls sig


dataIdwt :: [(Array U DIM1 Double, Array U DIM1 Double, Array U DIM1 Double)]
dataIdwt =
    [
      ( computeS . toRad $ fromListUnboxed (Z :. 3) [40,25,30], 
        fromListUnboxed (Z :. 16)
        [ -0.869330716850108, -1.33074602494193, -4.452066284456580, 
          -0.766339042879150, -3.99023927679201,  3.273575105871030, 
          -2.639689358691720, -1.39229920071584,  0.062440000137053, 
          -1.159888007129840,  0.97906335585356,  0.763494159561419, 
          -4.563606712907260, -4.76673895168943, -4.662257981490680, 
          -5.417080918602780 ],
       fromListUnboxed (Z :. 16) [1,2,2,4,-3,5,0,1,1,-1,-2,2,4,5,6,3]
      ),
      ( fromListUnboxed (Z :.  0) []
      , fromListUnboxed (Z :. 16) [1.0,2,2,4,-3,5,0,1,1,-1,-2,2,4,5,6,3]
      , fromListUnboxed (Z :. 16) [1.0,2,2,4,-3,5,0,1,1,-1,-2,2,4,5,6,3] 
      ),
      ( fromListUnboxed (Z :. 0) []
      , fromListUnboxed (Z :. 0) []
      , fromListUnboxed (Z :. 0) []
      ),
      ( fromListUnboxed (Z :. 3) [1,2,3]
      , fromListUnboxed (Z :. 0) []
      , fromListUnboxed (Z :. 0) []
      )
    ]


propDWTInvertible :: DwtInputRepa -> Bool
propDWTInvertible (DwtInputRepa (ls, sig)) = 
    idwt (computeS $ inv ls) (dwt ls sig) =~ sig


propDWTIdenticalToC :: DwtInputRepa -> Bool
propDWTIdenticalToC (DwtInputRepa (ls, sig)) = 
    cDwt =~ repaDwt
        where
          cDwt    = C.dwt (asV ls) (asV sig)
          repaDwt = asV $ dwt ls sig
          asV     = V.fromList . R.toList


propIDWTIdenticalToC :: DwtInputRepa -> Bool
propIDWTIdenticalToC (DwtInputRepa (ls, sig)) = 
    cIdwt =~ repaIdwt
        where
          cIdwt    = C.idwt (asV ls) (asV sig)
          repaIdwt = asV $ idwt ls sig
          asV      = V.fromList . R.toList
