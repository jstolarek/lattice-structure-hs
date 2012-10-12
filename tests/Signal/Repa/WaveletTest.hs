module Signal.Repa.WaveletTest where

import Test.QuickCheck

import Data.Array.Repa
import Signal.Repa.Wavelet

propPairsIdentity1 :: Property
propPairsIdentity1 =
    forAll genRepaUnboxedArray (\xs -> 
        (even . size . extent $ xs) ==>
             computeS (fromPairs (toPairs xs)) == xs)

propPairsIdentity2 :: Property
propPairsIdentity2 = forAll genRepaUnboxedArray2 (\xs ->
    computeS ( toPairs (fromPairs xs)) == xs)

genRepaUnboxedArray :: Gen (Array U DIM1 Double)
genRepaUnboxedArray = do
      randomList <- listOf1 arbitrary :: Gen [Double]
      let listLength = length randomList
      return $ fromListUnboxed ( Z :. listLength ) randomList

genRepaUnboxedArray2 :: Gen (Array U DIM1 (Double,Double))
genRepaUnboxedArray2 = do
      ds <- listOf1 arbitrary :: Gen [Double]
      let listLength = length ds
      return $ fromListUnboxed ( Z :. listLength ) $ zip ds (reverse ds)
