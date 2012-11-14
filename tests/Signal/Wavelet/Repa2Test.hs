module Signal.Wavelet.Repa2Test where

import Control.Arrow ((&&&))
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
     ( computeS . toRad $ fromListUnboxed (Z :. 3) [30,25,40]
     , fromListUnboxed (Z :. 16) [1,2,2,4,-3,5,0,1,1,-1,-2,2,4,5,6,3]
     , fromListUnboxed (Z :. 16) 
       [ -4.4520662844565800, -0.766339042879150, -3.990239276792010,  
          3.2735751058710300, -2.639689358691720, -1.392299200715840,
          0.0624400001370536, -1.159888007129840,  0.979063355853563,  
          0.7634941595614190, -4.563606712907260, -4.766738951689430, 
         -4.6622579814906800, -5.417080918602780, -0.869330716850108, 
         -1.3307460249419300 ]
     ),
     ( computeS . toRad $ fromListUnboxed (Z :. 1) [30]
     , fromListUnboxed (Z :. 16) [1,2,2,4,-3,5,0,1,1,-1,-2,2,4,5,6,3]
     , fromListUnboxed (Z :. 16) 
           [ 1.8660254038, -1.2320508076,  3.7320508076, -2.4641016151,
            -0.0980762114, -5.8301270189,  0.5000000000, -0.8660254038,
             0.3660254038,  1.3660254038, -0.7320508076, -2.7320508076,
             5.9641016151, -2.3301270189,  6.6961524227,  0.4019237886 ]

     ),
     ( fromListUnboxed (Z :.  0) []
     , fromListUnboxed (Z :. 16) [1.0,2,2,4,-3,5,0,1,1,-1,-2,2,4,5,6,3]
     , fromListUnboxed (Z :. 16) [1.0,2,2,4,-3,5,0,1,1,-1,-2,2,4,5,6,3]
     ),
     ( fromListUnboxed (Z :.  0) []
     , fromListUnboxed (Z :.  0) []
     , fromListUnboxed (Z :.  0) []
     ),
     ( fromListUnboxed (Z :.  3) [1,2,3]
     , fromListUnboxed (Z :.  0) []
     , fromListUnboxed (Z :.  0) []
     )
   ]


testIdwt :: (Array U DIM1 Double, Array U DIM1 Double, Array U DIM1 Double)
        -> Assertion
testIdwt (ls, sig, expected) = 
    expected @=~? idwt ls sig


dataIdwt :: [(Array U DIM1 Double, Array U DIM1 Double, Array U DIM1 Double)]
dataIdwt =
    [
     ( computeS . toRad $ fromListUnboxed (Z :. 3)  [40,25,30]
     , fromListUnboxed (Z :. 16) 
       [ -4.4520662844565800, -0.766339042879150, -3.990239276792010,  
          3.2735751058710300, -2.639689358691720, -1.392299200715840,
          0.0624400001370536, -1.159888007129840,  0.979063355853563,  
          0.7634941595614190, -4.563606712907260, -4.766738951689430, 
         -4.6622579814906800, -5.417080918602780, -0.869330716850108, 
         -1.3307460249419300 ]
     , fromListUnboxed (Z :. 16) [1,2,2,4,-3,5,0,1,1,-1,-2,2,4,5,6,3]
     ),
     ( computeS . toRad $ fromListUnboxed (Z :. 1) [30]
     , fromListUnboxed (Z :. 16) 
           [ 1.8660254038, -1.2320508076,  3.7320508076, -2.4641016151,
            -0.0980762114, -5.8301270189,  0.5000000000, -0.8660254038,
             0.3660254038,  1.3660254038, -0.7320508076, -2.7320508076,
             5.9641016151, -2.3301270189,  6.6961524227,  0.4019237886 ]
     , fromListUnboxed (Z :. 16) [1,2,2,4,-3,5,0,1,1,-1,-2,2,4,5,6,3]
     ),
     ( fromListUnboxed (Z :.  0) []
     , fromListUnboxed (Z :. 16) [1.0,2,2,4,-3,5,0,1,1,-1,-2,2,4,5,6,3]
     , fromListUnboxed (Z :. 16) [1.0,2,2,4,-3,5,0,1,1,-1,-2,2,4,5,6,3]
     ),
     ( fromListUnboxed (Z :.  0) []
     , fromListUnboxed (Z :.  0) []
     , fromListUnboxed (Z :.  0) []
     ),
     ( fromListUnboxed (Z :.  3) [1,2,3]
     , fromListUnboxed (Z :.  0) []
     , fromListUnboxed (Z :.  0) []
     )
    ]


propDWTInvertible :: DwtInputRepa -> Bool
propDWTInvertible (DwtInputRepa (ls, sig)) = 
    idwt (computeS $ inv ls) (dwt ls sig) =~ sig


testLattice :: ((Double, Double), Array U DIM1 Double, Array U DIM1 Double)
             -> Assertion
testLattice (baseOp, sig, expected) = 
    expected @=~? computeS (lattice baseOp sig)


dataLattice :: [((Double,Double), Array U DIM1 Double, Array U DIM1 Double)]
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
        fromListUnboxed (Z :.  0) [],
        fromListUnboxed (Z :.  0) [] )
    ]


propDoubleLatticeIdentity :: DwtInputRepa -> Bool
propDoubleLatticeIdentity (DwtInputRepa (ls, sig)) =
    computeS (lattice baseOp (computeS $ lattice baseOp sig)) =~ sig
        where
          baseOp = (sin &&& cos) $ ls ! (Z :. 0)


propDWTIdenticalToRepa1 :: DwtInputRepa -> Bool
propDWTIdenticalToRepa1 (DwtInputRepa (ls, sig)) = 
    R1.dwt ls sig =~ dwt ls sig


propIDWTIdenticalToRepa1 :: DwtInputRepa -> Bool
propIDWTIdenticalToRepa1 (DwtInputRepa (ls, sig)) = 
    R1.idwt ls sig =~ idwt ls sig


testExtendFront :: (Array U DIM1 Double, Int, Array U DIM1 Double)
              -> Assertion
testExtendFront (sig, ln, expected) = 
    expected @=~? (computeS . extendFront ln $ sig)


dataExtendFront :: [(Array U DIM1 Double, Int, Array U DIM1 Double)]
dataExtendFront =
   [
     ( fromListUnboxed (Z :. 12) [1,2,2,4,-3,5,0,1,1,-1,-2,2], 
       3,
       fromListUnboxed (Z :. 16) [1,-1,-2,2,1,2,2,4,-3,5,0,1,1,-1,-2,2]
     ),
     ( fromListUnboxed (Z :.  4) [1,2,3,4], 
       6,
       fromListUnboxed (Z :. 14) [3,4,1,2,3,4,1,2,3,4,1,2,3,4]
     ),
     ( fromListUnboxed (Z :.  2) [1,2], 
       3,
       fromListUnboxed (Z :.  6) [1,2,1,2,1,2]
     ),
     ( fromListUnboxed (Z :.  2) [1,2], 
       1,
       fromListUnboxed (Z :.  2) [1,2]
     ),
     ( fromListUnboxed (Z :.  0) [], 
       7,
       fromListUnboxed (Z :.  0) []
     )
   ]


testExtendEnd :: (Array U DIM1 Double, Int, Array U DIM1 Double)
              -> Assertion
testExtendEnd (sig, ln, expected) = 
    expected @=~? (computeS . extendEnd ln $ sig)


dataExtendEnd :: [(Array U DIM1 Double, Int, Array U DIM1 Double)]
dataExtendEnd =
   [
     ( fromListUnboxed (Z :. 12) [1,2,2,4,-3,5,0,1,1,-1,-2,2], 
       3,
       fromListUnboxed (Z :. 16) [1,2,2,4,-3,5,0,1,1,-1,-2,2,1,2,2,4]
     ),
     ( fromListUnboxed (Z :.  2) [1,2], 
       3,
       fromListUnboxed (Z :.  6) [1,2,1,2,1,2]
     ),
     ( fromListUnboxed (Z :.  2) [1,2], 
       1,
       fromListUnboxed (Z :.  2) [1,2]
     ),
     ( fromListUnboxed (Z :.  0) [], 
       7,
       fromListUnboxed (Z :.  0) []
     )
   ]


testTrim :: (Array U DIM1 Double, Array U DIM1 Double)
         -> Assertion
testTrim (sig, expected) = 
    expected @=~? (computeS . trim . delay $ sig)


dataTrim :: [(Array U DIM1 Double, Array U DIM1 Double)]
dataTrim =
   [
     ( fromListUnboxed (Z :. 16) [1,2,2,4,-3,5,0,1,1,-1,-2,2,4,5,6,3], 
       fromListUnboxed (Z :. 14) [  2,2,4,-3,5,0,1,1,-1,-2,2,4,5,6  ]
     ),
     ( fromListUnboxed (Z :.  2) [1,2],
       fromListUnboxed (Z :.  0) []
     ),
     ( fromListUnboxed (Z :.  1) [1],
       fromListUnboxed (Z :.  0) []
     ),
     ( fromListUnboxed (Z :.  0) [],
       fromListUnboxed (Z :.  0) []
     )
   ]