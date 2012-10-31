{-# LANGUAGE FlexibleContexts #-}

module Signal.Wavelet.Repa1Test where

import Data.Array.Repa as R
import Signal.Wavelet.Repa.Common
import Signal.Wavelet.Repa1
import Test.ArbitraryInstances
import Test.HUnit
import Test.QuickCheck
import Test.Utils


testDwt :: (Array U DIM1 Double, Array U DIM1 Double, Array U DIM1 Double)
        -> Assertion
testDwt (ls, sig, expected) = 
    expected @=~? dwt ls sig


dataDwt :: [(Array U DIM1 Double, Array U DIM1 Double, Array U DIM1 Double)]
dataDwt =
    [
      ( computeS . toRad $ fromListUnboxed (Z :. (3::Int))  [30,25,40], 
        fromListUnboxed (Z :. (16::Int)) $ [ 1,2,2,4,-3,5,0,1,1,-1,-2,2,4,5,6,3], 
        fromListUnboxed (Z :. (16::Int)) 
        [ -4.4520662844565800, -0.766339042879150, -3.990239276792010,  
          3.2735751058710300, -2.639689358691720, -1.392299200715840,
          0.0624400001370536, -1.159888007129840,  0.979063355853563,  
          0.7634941595614190, -4.563606712907260, -4.766738951689430, 
         -4.6622579814906800, -5.417080918602780, -0.869330716850108, 
         -1.3307460249419300 ] 
      ),
      ( fromListUnboxed (Z :. ( 0::Int)) $ [], 
        fromListUnboxed (Z :. (16::Int)) $ [1.0,2,2,4,-3,5,0,1,1,-1,-2,2,4,5,6,3],
        fromListUnboxed (Z :. (16::Int)) $ [1.0,2,2,4,-3,5,0,1,1,-1,-2,2,4,5,6,3] 
      ),
      ( fromListUnboxed (Z :. (0::Int)) $ [],
        fromListUnboxed (Z :. (0::Int)) $ [],
        fromListUnboxed (Z :. (0::Int)) $ []
      ),
      ( fromListUnboxed (Z :. (3::Int)) $ [1,2,3],
        fromListUnboxed (Z :. (0::Int)) $ [],
        fromListUnboxed (Z :. (0::Int)) $ []
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
        [ -4.4520662844565800, -0.766339042879150, -3.990239276792010,  
           3.2735751058710300, -2.639689358691720, -1.392299200715840,
           0.0624400001370536, -1.159888007129840,  0.979063355853563,  
           0.7634941595614190, -4.563606712907260, -4.766738951689430, 
          -4.6622579814906800, -5.417080918602780, -0.869330716850108, 
          -1.3307460249419300 ],
       fromListUnboxed (Z :. 16) [1,2,2,4,-3,5,0,1,1,-1,-2,2,4,5,6,3]
      ),
      ( fromListUnboxed (Z :.  0) [], 
        fromListUnboxed (Z :. 16) [1.0,2,2,4,-3,5,0,1,1,-1,-2,2,4,5,6,3],
        fromListUnboxed (Z :. 16) [1.0,2,2,4,-3,5,0,1,1,-1,-2,2,4,5,6,3] 
      ),
      ( fromListUnboxed (Z :. 0) [],
        fromListUnboxed (Z :. 0) [],
        fromListUnboxed (Z :. 0) []
      ),
      ( fromListUnboxed (Z :. 3) [1,2,3],
        fromListUnboxed (Z :. 0) [],
        fromListUnboxed (Z :. 0) []
      )
    ]


propDWTInvertible :: DwtInputRepa -> Bool
propDWTInvertible (DwtInputRepa (ls, sig)) = 
    idwt (computeS $ inv ls) (dwt ls sig) =~ sig


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
        fromListUnboxed (Z :. 16) 
            [ 1, 2, 2, 4,-3, 5, 0, 1, 
              1,-1,-2, 2, 4, 5, 6, 3 ],
        fromListUnboxed (Z :. 16) 
            [ 1.8660254038, -1.2320508076,  3.7320508076, -2.4641016151,
             -0.0980762114, -5.8301270189,  0.5000000000, -0.8660254038,
              0.3660254038,  1.3660254038, -0.7320508076, -2.7320508076,
              5.9641016151, -2.3301270189,  6.6961524227,  0.4019237886 ]
      ), ( (0.5, 0.8660254038), 
        fromListUnboxed (Z :. 0) [],
        fromListUnboxed (Z :. 0) [] )
    ]


propDoubleLatticeInverse :: RepaDIM1Array -> Bool
propDoubleLatticeInverse (RepaDIM1Array ls) = 
    computeS (inv . inv $ ls) == ls


testCsl :: (Array U DIM1 Double, Array U DIM1 Double) -> Assertion
testCsl (input, expected) = 
    expected @=? (computeS $ csl input)


dataCsl :: [(Array U DIM1 Double, Array U DIM1 Double)]
dataCsl = 
    [
     ( fromListUnboxed (Z :. 4) [1,2,3,4], fromListUnboxed (Z :. 4) [2,3,4,1] ),
     ( fromListUnboxed (Z :. 0) []       , fromListUnboxed (Z :. 0) []        )
    ]


testCsr :: (Array U DIM1 Double, Array U DIM1 Double) -> Assertion
testCsr (input, expected) = 
    expected @=? (computeS $ csr input)


dataCsr :: [(Array U DIM1 Double, Array U DIM1 Double)]
dataCsr = 
    [
     ( fromListUnboxed (Z :. 4) [1,2,3,4], fromListUnboxed (Z :. 4) [4,1,2,3] ),
     ( fromListUnboxed (Z :. 0) []       , fromListUnboxed (Z :. 0) []        )
    ]


testCslN :: (Int, Array U DIM1 Double, Array U DIM1 Double) -> Assertion
testCslN (n, input, expected) = 
    expected @=? (computeS $ cslN n input)


dataCslN :: [(Int, Array U DIM1 Double, Array U DIM1 Double)]
dataCslN = 
    [
     ( 1, 
       fromListUnboxed (Z :. 4) [1,2,3,4], 
       fromListUnboxed (Z :. 4) [2,3,4,1]
     ),
     ( 0, 
       fromListUnboxed (Z :. 4) [1,2,3,4], 
       fromListUnboxed (Z :. 4) [1,2,3,4] 
     ),
     ( 4, 
       fromListUnboxed (Z :. 0) [], 
       fromListUnboxed (Z :. 0) []
     )
    ]


testCsrN :: (Int, Array U DIM1 Double, Array U DIM1 Double) -> Assertion
testCsrN (n, input, expected) = 
    expected @=? (computeS $ csrN n input)


dataCsrN :: [(Int, Array U DIM1 Double, Array U DIM1 Double)]
dataCsrN = 
    [
     ( 1, 
       fromListUnboxed (Z :. 4) [1,2,3,4], 
       fromListUnboxed (Z :. 4) [4,1,2,3] 
     ),
     ( 0, 
       fromListUnboxed (Z :. 4) [1,2,3,4], 
       fromListUnboxed (Z :. 4) [1,2,3,4]
     ),
     ( 4, 
       fromListUnboxed (Z :. 0) [], 
       fromListUnboxed (Z :. 0) []
     )
    ]


propIdentityShift1 :: RepaDIM1Array -> Bool
propIdentityShift1 (RepaDIM1Array xs) = 
    computeS (csl . csr $ xs) == xs


propIdentityShift2 :: RepaDIM1Array -> Bool
propIdentityShift2 (RepaDIM1Array xs) = 
    computeS (csr . csl $ xs) == xs


propIdentityShift3 :: Int -> RepaDIM1Array -> Bool
propIdentityShift3 n (RepaDIM1Array xs) =
    computeS (cslN n . csrN n $ xs) == xs


propIdentityShift4 :: Int -> RepaDIM1Array -> Bool
propIdentityShift4 n (RepaDIM1Array xs) =
    computeS (csrN n . cslN n $ xs) == xs


propIdentityShift5 :: RepaDIM1Array -> Bool
propIdentityShift5 (RepaDIM1Array xs) = 
    computeS (cslN n xs) == xs
        where n = size . extent $ xs


propIdentityShift6 :: RepaDIM1Array -> Bool
propIdentityShift6 (RepaDIM1Array xs) = 
    computeS (csrN n xs) == xs
        where n = size . extent $ xs


propPairsIdentity1 :: RepaDIM1Array -> Property
propPairsIdentity1 (RepaDIM1Array xs) =
    (even . size . extent $ xs) ==>
        computeS (fromPairs . toPairs $ xs) == xs


propPairsIdentity2 :: RepaDIM1Array -> Bool
propPairsIdentity2 (RepaDIM1Array xs) = 
    computeS (toPairs . fromPairs $ ys) == ys
        where 
          ys :: (Source U (Double, Double)) => Array U DIM1 (Double,Double)
          ys = computeS $ R.zipWith (,) xs xs
