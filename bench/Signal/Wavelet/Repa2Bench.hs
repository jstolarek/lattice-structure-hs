module Signal.Wavelet.Repa2Bench where

import Data.Array.Repa
import Data.Array.Repa.Algorithms.Randomish
import Signal.Wavelet.Repa2
import System.Random


{-# INLINE benchDwt #-}
benchDwt :: (Array U DIM1 Double, Array U DIM1 Double) -> Array U DIM1 Double
benchDwt (ls, sig) = dwt ls sig


{-# INLINE benchIdwt #-}
benchIdwt :: (Array U DIM1 Double, Array U DIM1 Double) -> Array U DIM1 Double
benchIdwt (ls, sig) = idwt ls sig


dataDwt :: RandomGen g 
        => g 
        -> Int 
        -> Int 
        -> (Array U DIM1 Double, Array U DIM1 Double)
dataDwt gen lsSize sigSize = (randomishDoubleArray (Z :. lsSize ) 0 255 seed, 
                              randomishDoubleArray (Z :. sigSize) 0 255 seed)
    where
      seed = fst . next $ gen
