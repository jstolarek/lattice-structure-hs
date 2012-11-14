module Signal.Wavelet.List2Test where

import qualified Signal.Wavelet.List1 as L1
import Signal.Wavelet.List2
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


propDWTIdenticalToList1 :: DwtInputList -> Bool
propDWTIdenticalToList1 (DwtInputList (ls, sig)) = 
    L1.dwt ls sig =~ dwt ls sig


propIDWTIdenticalToList1 :: DwtInputList -> Bool
propIDWTIdenticalToList1 (DwtInputList (ls, sig)) = 
    L1.idwt ls sig =~ idwt ls sig


testExtendFront :: ([Double], Int, [Double]) -> Assertion
testExtendFront (sig, ln, expected) = 
    expected @=~? extendFront ln sig


dataExtendFront :: [([Double], Int, [Double])]
dataExtendFront =
   [
     ( [1,2,2,4,-3,5,0,1,1,-1,-2,2], 
       3,
       [1,-1,-2,2,1,2,2,4,-3,5,0,1,1,-1,-2,2]
     ),
     ( [1,2,3,4], 
       6,
       [3,4,1,2,3,4,1,2,3,4,1,2,3,4]
     ),
     ( [1,2], 
       3,
       [1,2,1,2,1,2]
     ),
     ( [1,2], 
       1,
       [1,2]
     ),
     ( [], 
       7,
       []
     )
   ]


testExtendEnd :: ([Double], Int, [Double]) -> Assertion
testExtendEnd (sig, ln, expected) = 
    expected @=~? extendEnd ln sig


dataExtendEnd :: [([Double], Int, [Double])]
dataExtendEnd =
   [
     ( [1,2,2,4,-3,5,0,1,1,-1,-2,2], 
       3,
       [1,2,2,4,-3,5,0,1,1,-1,-2,2,1,2,2,4]
     ),
     ( [1,2], 
       3,
       [1,2,1,2,1,2]
     ),
     ( [1,2], 
       1,
       [1,2]
     ),
     ( [], 
       7,
       []
     )
   ]
