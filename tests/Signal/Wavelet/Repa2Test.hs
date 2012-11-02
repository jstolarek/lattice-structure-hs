module Signal.Wavelet.Repa2Test where

import Data.Array.Repa as R
import qualified Signal.Wavelet.Repa1 as R1
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
       [ -4.4520662844565800, -0.766339042879150, -3.990239276792010,  
          3.2735751058710300, -2.639689358691720, -1.392299200715840,
          0.0624400001370536, -1.159888007129840,  0.979063355853563,  
          0.7634941595614190, -4.563606712907260, -4.766738951689430, 
         -4.6622579814906800, -5.417080918602780, -0.869330716850108, 
         -1.3307460249419300 ]
     ),
     ( computeS . toRad $ fromListUnboxed (Z :. (1::Int)) [30]
     , fromListUnboxed (Z :. 16) [1,2,2,4,-3,5,0,1,1,-1,-2,2,4,5,6,3]
     , fromListUnboxed (Z :. 16) 
           [ 1.8660254038, -1.2320508076,  3.7320508076, -2.4641016151,
            -0.0980762114, -5.8301270189,  0.5000000000, -0.8660254038,
             0.3660254038,  1.3660254038, -0.7320508076, -2.7320508076,
             5.9641016151, -2.3301270189,  6.6961524227,  0.4019237886 ]

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


{-testIdwt :: (Array U DIM1 Double, Array U DIM1 Double, Array U DIM1 Double)
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
    idwt (computeS $ inv ls) (dwt ls sig) =~ sig-}


testLattice :: ((Double, Double), 
                Array U DIM1 Double,
                Array U DIM1 Double)
             -> Assertion
testLattice (baseOp, sig, expected) = 
    expected @=~? computeS (lattice baseOp sig)


dataLattice :: [((Double, Double), 
                 Array U DIM1 Double,
                 Array U DIM1 Double)]
dataLattice =
    [
      ( (0.5, 0.8660254038),
        fromListUnboxed (Z :. 16) [1,2,2,4,-3,5,0,1,1,-1,-2,2,4,5,6,3],
        fromListUnboxed (Z :. 16) 
            [ 1.8660254038, -1.2320508076,  3.7320508076, -2.4641016151,
             -0.0980762114, -5.8301270189,  0.5000000000, -0.8660254038,
              0.3660254038,  1.3660254038, -0.7320508076, -2.7320508076,
              5.9641016151, -2.3301270189,  6.6961524227,  0.4019237886 ]
      ), 
      ( (0.5, 0.8660254038), 
        fromListUnboxed (Z :. 0) [],
        fromListUnboxed (Z :. 0) [] )
    ]


propDWTIdenticalToRepa1 :: DwtInputRepa -> Bool
propDWTIdenticalToRepa1 (DwtInputRepa (ls, sig)) = 
    R1.dwt ls sig =~ dwt ls sig


{-propIDWTIdenticalToC :: DwtInputRepa -> Bool
propIDWTIdenticalToC (DwtInputRepa (ls, sig)) = 
    cIdwt =~ repaIdwt
        where
          cIdwt    = C.idwt (asV ls) (asV sig)
          repaIdwt = asV $ idwt ls sig
          asV      = V.fromList . R.toList
-}

testExtendSignal :: (Array U DIM1 Double, Int, Array U DIM1 Double)
                 -> Assertion
testExtendSignal (sig, ln, expected) = 
    expected @=~? (computeS $ extendSignal ln (delay sig))


dataExtendSignal :: [(Array U DIM1 Double, Int, Array U DIM1 Double)]
dataExtendSignal =
   [
     ( fromListUnboxed (Z :. (12::Int)) $ [1,2,2,4,-3,5,0,1,1,-1,-2,2], 
       3,
       fromListUnboxed (Z :. (16::Int)) $ [1,2,2,4,-3,5,0,1,1,-1,-2,2,1,2,2,4]
     ),
     ( fromListUnboxed (Z :. (2::Int)) $ [1,2], 
       3,
       fromListUnboxed (Z :. (6::Int)) $ [1,2,1,2,1,2]
     ),
     ( fromListUnboxed (Z :. (2::Int)) $ [1,2], 
       1,
       fromListUnboxed (Z :. (2::Int)) $ [1,2]
     ),
     ( fromListUnboxed (Z :. (0::Int)) $ [], 
       7,
       fromListUnboxed (Z :. (0::Int)) $ []
     )
   ]


testTrim :: (Array U DIM1 Double, Array U DIM1 Double)
         -> Assertion
testTrim (sig, expected) = 
    expected @=~? (computeS $ trim sig)


dataTrim :: [(Array U DIM1 Double, Array U DIM1 Double)]
dataTrim =
   [
     ( fromListUnboxed (Z :. (16::Int)) $ [1,2,2,4,-3,5,0,1,1,-1,-2,2,4,5,6,3], 
       fromListUnboxed (Z :. (14::Int)) $ [  2,2,4,-3,5,0,1,1,-1,-2,2,4,5,6  ]
     ),
     ( fromListUnboxed (Z :. (2 ::Int)) $ [1,2],
       fromListUnboxed (Z :. (0 ::Int)) $ []
     ),
     ( fromListUnboxed (Z :. (1 ::Int)) $ [1],
       fromListUnboxed (Z :. (0 ::Int)) $ []
     ),
     ( fromListUnboxed (Z :. (0 ::Int)) $ [],
       fromListUnboxed (Z :. (0 ::Int)) $ []
     )
   ]
