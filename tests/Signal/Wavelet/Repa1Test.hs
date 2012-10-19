module Signal.Wavelet.Repa1Test where

import Data.Array.Repa
import Signal.Wavelet.Repa.Common
import Signal.Wavelet.Repa1
import Test.HUnit
import Test.QuickCheck
import Test.Repa
import Test.Utils

testDwt :: (Array U DIM1 Double, Array U DIM1 Double, Array U DIM1 Double)
        -> Assertion
testDwt (ls, sig, expected) = 
    expected @=~? dwt ls sig

dataDwt :: [(Array U DIM1 Double, Array U DIM1 Double, Array U DIM1 Double)]
dataDwt =
    [
     (computeS . toRad $ fromListUnboxed (Z :. (3::Int))  [30,25,40], 
      fromListUnboxed (Z :. (16::Int)) $ [ 1,2,2,4,-3,5,0,1,1,-1,-2,2,4,5,6,3], 
      fromListUnboxed (Z :. (16::Int)) 
      [ -4.4520662844565800, -0.766339042879150, -3.990239276792010,  
         3.2735751058710300, -2.639689358691720, -1.392299200715840,
         0.0624400001370536, -1.159888007129840,  0.979063355853563,  
         0.7634941595614190, -4.563606712907260, -4.766738951689430, 
        -4.6622579814906800, -5.417080918602780, -0.869330716850108, 
        -1.3307460249419300
       ] )
    ]

testIdwt :: (Array U DIM1 Double, Array U DIM1 Double, Array U DIM1 Double)
        -> Assertion
testIdwt (ls, sig, expected) = 
    expected @=~? idwt ls sig

dataIdwt :: [(Array U DIM1 Double, Array U DIM1 Double, Array U DIM1 Double)]
dataIdwt =
    [
      (computeS . toRad $ fromListUnboxed (Z :. (3::Int))  [40,25,30], 
      fromListUnboxed (Z :. (16::Int)) $
      [ -4.4520662844565800, -0.766339042879150, -3.990239276792010,  
         3.2735751058710300, -2.639689358691720, -1.392299200715840,
         0.0624400001370536, -1.159888007129840,  0.979063355853563,  
         0.7634941595614190, -4.563606712907260, -4.766738951689430, 
        -4.6622579814906800, -5.417080918602780, -0.869330716850108, 
        -1.3307460249419300
       ],
       fromListUnboxed (Z :. (16::Int)) [1,2,2,4,-3,5,0,1,1,-1,-2,2,4,5,6,3])
    ]

propDWTInvertible :: Property
propDWTInvertible = 
    forAll genRepaUnboxedArrayPair (\(xs, ls) ->
        (even . size . extent $ xs) ==>
                idwt (inv ls) (dwt ls xs) =~ xs)

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
      ((0.5, 0.8660254038), 
       fromListUnboxed (Z :. (16::Int)) 
           [ 1, 2, 2, 4,-3, 5, 0, 1, 
             1,-1,-2, 2, 4, 5, 6, 3 ],
       fromListUnboxed (Z :. (16::Int)) 
           [ 1.8660254038, -1.2320508076,  3.7320508076, -2.4641016151,
            -0.0980762114, -5.8301270189,  0.5000000000, -0.8660254038,
             0.3660254038,  1.3660254038, -0.7320508076, -2.7320508076,
             5.9641016151, -2.3301270189,  6.6961524227,  0.4019237886 ]
      )
    ]

propDoubleLatticeInverse :: Property
propDoubleLatticeInverse = 
    forAll genRepaUnboxedArray (\xs ->
        computeS (inv . inv $ xs) == xs)

testCsl :: (Array U DIM1 Double, Array U DIM1 Double) -> Assertion
testCsl (input, expected) = 
    expected @=? (computeS $ csl input)

dataCsl :: [(Array U DIM1 Double, Array U DIM1 Double)]
dataCsl = 
    [
     ( fromListUnboxed (Z :. 4) [1,2,3,4], fromListUnboxed (Z :. 4) [2,3,4,1] )
    ]

testCsr :: (Array U DIM1 Double, Array U DIM1 Double) -> Assertion
testCsr (input, expected) = 
    expected @=? (computeS $ csr input)

dataCsr :: [(Array U DIM1 Double, Array U DIM1 Double)]
dataCsr = 
    [
     ( fromListUnboxed (Z :. 4) [1,2,3,4], fromListUnboxed (Z :. 4) [4,1,2,3] )
    ]

propIdentityShift1 :: Property
propIdentityShift1 = 
    forAll genRepaUnboxedArray (\xs ->
        computeS (csl . csr $ xs) == xs)

propIdentityShift2 :: Property
propIdentityShift2 = 
    forAll genRepaUnboxedArray (\xs ->
        computeS (csr . csl $ xs) == xs)

propPairsIdentity1 :: Property
propPairsIdentity1 =
    forAll genRepaUnboxedArray (\xs -> 
        (even . size . extent $ xs) ==>
             computeS (fromPairs . toPairs $ xs) == xs)

propPairsIdentity2 :: Property
propPairsIdentity2 = 
    forAll genRepaUnboxedArray (\xs ->
        computeS (toPairs . fromPairs $ xs) == xs)
