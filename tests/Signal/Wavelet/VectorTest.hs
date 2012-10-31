module Signal.Wavelet.VectorTest where

import Control.Arrow ((&&&))
import Data.Vector.Unboxed as V
import qualified Signal.Wavelet.C as C
import Signal.Wavelet.Vector
import Signal.Wavelet.Vector.Common
import Test.ArbitraryInstances
import Test.HUnit
import Test.QuickCheck
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
      ( fromList [], 
        fromList [1,2,2,4,-3,5,0,1,1,-1,-2,2,4,5,6,3],
        fromList [1,2,2,4,-3,5,0,1,1,-1,-2,2,4,5,6,3] 
      ),
      ( fromList [],
        fromList [],
        fromList []
      ),
      ( fromList [1,2,3],
        fromList [],
        fromList []
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
      ( fromList [], 
        fromList [1,2,2,4,-3,5,0,1,1,-1,-2,2,4,5,6,3],
        fromList [1,2,2,4,-3,5,0,1,1,-1,-2,2,4,5,6,3] 
      ),
      ( fromList [],
        fromList [],
        fromList []
      ),
      ( fromList [1,2,3],
        fromList [],
        fromList []
      )
    ]


propDWTInvertible :: DwtInputVector -> Bool
propDWTInvertible (DwtInputVector (ls, sig)) = 
    idwt (inv ls) (dwt ls sig) =~ sig


testLattice :: (Int, (Double, Double), Vector Double, Vector Double) -> Assertion
testLattice (lm, baseOp, sig, expected) = 
    expected @=~? lattice lm baseOp sig


dataLattice :: [(Int, (Double, Double), Vector Double, Vector Double)]
dataLattice =
    [
      ( 0,
        (0.5, 0.8660254038), 
        fromList [ 1, 2, 2, 4,-3, 5, 0, 1, 1,-1,-2, 2, 4, 5, 6, 3 ],
        fromList [ 1.8660254038, -1.2320508076,  3.7320508076, -2.4641016151,
                  -0.0980762114, -5.8301270189,  0.5000000000, -0.8660254038,
                   0.3660254038,  1.3660254038, -0.7320508076, -2.7320508076,
                   5.9641016151, -2.3301270189,  6.6961524227,  0.4019237886 ]
      ), 
      ( 1,
        ( 0.4226182617, 0.9063077870 ),
        fromList [  1.8660254038, -1.2320508076,  3.7320508076, -2.4641016151,
                   -0.0980762114, -5.8301270189,  0.5000000000, -0.8660254038,
                    0.3660254038,  1.3660254038, -0.7320508076, -2.7320508076,
                    5.9641016151, -2.3301270189,  6.6961524227,  0.4019237886 ],
        fromList [ -1.5213330213,  0.4606155841, -3.9030738792, -2.2746832798, 
                   -0.9524871073, -5.0725803858, -2.9170720400, -0.6301965473,
                   -0.6977298245,  0.9286614209,  1.2407706290,  0.0444593360,
                   -6.5599262998,  0.7181040352, -7.0535293143,  1.1528830720 ]
      ),
      ( 0,
        (0.5, 0.8660254038), 
        fromList [], 
        fromList []
      )
    ]


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
          cDwt    = C.dwt (convert ls) (convert sig)
          repaDwt = convert $ dwt ls sig


propIDWTIdenticalToC :: DwtInputVector -> Bool
propIDWTIdenticalToC (DwtInputVector (ls, sig)) = 
    cIdwt =~ repaIdwt
        where
          cIdwt    = C.idwt (convert ls) (convert sig)
          repaIdwt = convert $ idwt ls sig
