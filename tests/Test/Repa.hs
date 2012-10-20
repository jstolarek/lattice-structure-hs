module Test.Repa where

import Control.Monad (liftM, liftM2)
import Data.Array.Repa
import Data.Vector.Unboxed (Unbox)
import Test.QuickCheck


genRepaUnboxedArray :: (Arbitrary a, Unbox a) => Gen (Array U DIM1 a)
genRepaUnboxedArray = do
    randomList <- listOf1 arbitrary
    let listLength = length randomList
    return $ fromListUnboxed ( Z :. listLength ) randomList


genRepaDelayedArray :: (Arbitrary a, Unbox a) => Gen (Array D DIM1 a)
genRepaDelayedArray = liftM delay genRepaUnboxedArray


genRepaUnboxedArrayPair :: (Arbitrary a, Unbox a, Arbitrary b, Unbox b) 
                           => Gen (Array U DIM1 a, Array U DIM1 b)
genRepaUnboxedArrayPair = liftM2 (,) genRepaUnboxedArray 
                           (resize 4 genRepaUnboxedArray)
