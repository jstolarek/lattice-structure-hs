module Signal.Repa.WaveletTest where

import Control.Monad
import Test.HUnit
import Test.QuickCheck
import Test.Utils

import Data.Array.Repa
import qualified Data.Vector.Unboxed as V
import Signal.Repa.Wavelet

testDwt :: (Array D DIM1 Double, Array D DIM1 Double, Array U DIM1 Double)
        -> Assertion
testDwt (ls, sig, expected) = 
    expected @=~? computeS (dwtR ls sig)

dataDwt :: [(Array D DIM1 Double, Array D DIM1 Double, Array U DIM1 Double)]
dataDwt =
    [
      (toRadR $ fromListUnboxed (Z :. (3::Int))  [30,25,40], 
      delay . fromListUnboxed (Z :. (16::Int)) $ 
                [ 1,2,2,4,-3,5,0,1,1,-1,-2,2,4,5,6,3], 
      fromListUnboxed (Z :. (16::Int)) 
      [ -4.4520662844565800, -0.766339042879150, -3.990239276792010,  
         3.2735751058710300, -2.639689358691720, -1.392299200715840,
         0.0624400001370536, -1.159888007129840,  0.979063355853563,  
         0.7634941595614190, -4.563606712907260, -4.766738951689430, 
        -4.6622579814906800, -5.417080918602780, -0.869330716850108, 
        -1.3307460249419300
       ] )
    ]

testIdwt :: (Array D DIM1 Double, Array D DIM1 Double, Array U DIM1 Double)
        -> Assertion
testIdwt (ls, sig, expected) = 
    expected @=~? computeS (idwtR ls sig)

dataIdwt :: [(Array D DIM1 Double, Array D DIM1 Double, Array U DIM1 Double)]
dataIdwt =
    [
      (toRadR $ fromListUnboxed (Z :. (3::Int))  [40,25,30], 
      delay . fromListUnboxed (Z :. (16::Int)) $
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
                computeS (idwtR (invLSR ls) (dwtR ls (delay xs))) =~ xs)

propPairsIdentity1 :: Property
propPairsIdentity1 =
    forAll genRepaUnboxedArray (\xs -> 
        (even . size . extent $ xs) ==>
             computeS (fromPairsR (toPairsR xs)) == xs)

propPairsIdentity2 :: Property
propPairsIdentity2 = 
    forAll genRepaUnboxedArray (\xs ->
        computeS (toPairsR (fromPairsR xs)) == xs)

propDoubleLatticeInverse :: Property
propDoubleLatticeInverse = 
    forAll genRepaUnboxedArray (\xs ->
        computeS (invLSR (invLSR xs)) == xs)

propIdentityShift1 :: Property
propIdentityShift1 = 
    forAll genRepaUnboxedArray (\xs ->
        computeS (cyclicShiftLeftR (cyclicShiftRightR xs)) == xs)

propIdentityShift2 :: Property
propIdentityShift2 = 
    forAll genRepaUnboxedArray (\xs ->
        computeS (cyclicShiftRightR (cyclicShiftLeftR xs)) == xs)

testCyclicShiftLeft :: (Array U DIM1 Double, Array U DIM1 Double) -> Assertion
testCyclicShiftLeft (input, expected) = 
    expected @=? (computeS $ cyclicShiftLeftR input)

testCyclicShiftRight :: (Array U DIM1 Double, Array U DIM1 Double) -> Assertion
testCyclicShiftRight (input, expected) = 
    expected @=? (computeS $ cyclicShiftRightR input)

dataProviderCyclicShiftLeft :: [(Array U DIM1 Double, Array U DIM1 Double)]
dataProviderCyclicShiftLeft = 
    [
     ( fromListUnboxed (Z :. 4) [1,2,3,4], fromListUnboxed (Z :. 4) [2,3,4,1] )
    ]

dataProviderCyclicShiftRight :: [(Array U DIM1 Double, Array U DIM1 Double)]
dataProviderCyclicShiftRight = 
    [
     ( fromListUnboxed (Z :. 4) [1,2,3,4], fromListUnboxed (Z :. 4) [4,1,2,3] )
    ]

genRepaUnboxedArray :: (Arbitrary a, V.Unbox a) => Gen (Array U DIM1 a)
genRepaUnboxedArray = do
    randomList <- listOf1 arbitrary
    let listLength = length randomList
    return $ fromListUnboxed ( Z :. listLength ) randomList

genRepaDelayedArray :: (Arbitrary a, V.Unbox a) => Gen (Array D DIM1 a)
genRepaDelayedArray = liftM delay genRepaUnboxedArray

genRepaUnboxedArrayPair :: (Arbitrary a, V.Unbox a, Arbitrary b, V.Unbox b) 
                           => Gen (Array U DIM1 a, Array U DIM1 b)
genRepaUnboxedArrayPair = liftM2 (,) (resize 4 genRepaUnboxedArray) (resize 4 genRepaUnboxedArray)

testLatticeLayer :: ((Double, Double), 
                     Array U DIM1 Double,
                     Array U DIM1 Double)
                  -> Assertion
testLatticeLayer (baseOperation, sig, expected) = 
    expected @=~? computeS (latticeLayerR baseOperation sig)

dataLatticeLayer :: [((Double, Double), 
                      Array U DIM1 Double,
                      Array U DIM1 Double)] 
dataLatticeLayer =
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

propDegRadInvertible :: Property
propDegRadInvertible = 
    forAll genRepaUnboxedArray (\xs ->
        computeS (toDegR (toRadR xs)) =~ xs)

propRadDegInvertible :: Property
propRadDegInvertible =
    forAll genRepaUnboxedArray (\xs ->
        computeS (toRadR (toDegR xs)) =~ xs)
