module Signal.Wavelet.List1Test where

import Control.Arrow ((&&&))
import Signal.Wavelet.List1
import Signal.Wavelet.List.Common
import Test.ArbitraryInstances
import Test.HUnit
import Test.Utils


testDwt :: ([Double], [Double], [Double]) -> Assertion
testDwt (ls, sig, expected) = 
    expected @=~? dwt ls sig


dataDwt :: [([Double], [Double], [Double])] 
dataDwt =
    [
      ( toRad [30,25,40], 
        [1,2,2,4,-3,5,0,1,1,-1,-2,2,4,5,6,3], 
        [ -4.4520662844565800, -0.766339042879150, -3.990239276792010,  
           3.2735751058710300, -2.639689358691720, -1.392299200715840,
           0.0624400001370536, -1.159888007129840,  0.979063355853563,  
           0.7634941595614190, -4.563606712907260, -4.766738951689430, 
          -4.6622579814906800, -5.417080918602780, -0.869330716850108, 
          -1.3307460249419300 ] 
      ),
      ( [], 
        [1,2,2,4,-3,5,0,1,1,-1,-2,2,4,5,6,3],
        [1,2,2,4,-3,5,0,1,1,-1,-2,2,4,5,6,3] 
      ),
      ( [],
        [],
        []
      ),
      ( [1,2,3],
        [],
        []
      )
    ]


testIdwt :: ([Double], [Double], [Double]) -> Assertion
testIdwt (ls, sig, expected) = 
    expected @=~? idwt ls sig


dataIdwt :: [([Double], [Double], [Double])] 
dataIdwt =
    [
      ( toRad [40,25,30], 
        [ -4.4520662844565800, -0.766339042879150, -3.990239276792010,  
           3.2735751058710300, -2.639689358691720, -1.392299200715840,
           0.0624400001370536, -1.159888007129840,  0.979063355853563,  
           0.7634941595614190, -4.563606712907260, -4.766738951689430, 
          -4.6622579814906800, -5.417080918602780, -0.869330716850108, 
          -1.3307460249419300 ], 
        [1,2,2,4,-3,5,0,1,1,-1,-2,2,4,5,6,3] 
      ),
      ( [], 
        [1,2,2,4,-3,5,0,1,1,-1,-2,2,4,5,6,3],
        [1,2,2,4,-3,5,0,1,1,-1,-2,2,4,5,6,3] 
      ),
      ( [],
        [],
        []
      ),
      ( [1,2,3],
        [],
        []
      )
    ]


propDWTInvertible :: DwtInputList -> Bool
propDWTInvertible (DwtInputList (ls, sig)) = 
    idwt (inv ls) (dwt ls sig) =~ sig


testLattice :: ((Double, Double), [Double], [Double]) -> Assertion
testLattice (baseOp, sig, expected) = 
    expected @=~? lattice baseOp sig


dataLattice :: [((Double, Double), [Double], [Double])]
dataLattice =
    [
      (
       (0.5, 0.8660254038), 
       [ 1, 2, 2, 4,-3, 5, 0, 1, 1,-1,-2, 2, 4, 5, 6, 3 ],
       [ 1.8660254038, -1.2320508076,  3.7320508076, -2.4641016151,
        -0.0980762114, -5.8301270189,  0.5000000000, -0.8660254038,
         0.3660254038,  1.3660254038, -0.7320508076, -2.7320508076,
         5.9641016151, -2.3301270189,  6.6961524227,  0.4019237886 ]
      ), (
       (0.5, 0.8660254038), 
       [], 
       []
      )
    ]


propDoubleLatticeIdentity :: DwtInputList -> Bool
propDoubleLatticeIdentity (DwtInputList (ls, sig)) =
    lattice baseOp (lattice baseOp sig) =~ sig
        where
          baseOp = (sin &&& cos) $ head ls
