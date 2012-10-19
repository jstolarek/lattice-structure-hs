module Signal.Wavelet.Repa1Bench where

import BenchParam
import Data.Array.Repa
import Data.Array.Repa.Algorithms.Randomish
import System.Random

import Signal.Wavelet.Repa1

benchDwt :: (Array U DIM1 Double, Array U DIM1 Double) -> Array U DIM1 Double
benchDwt (ls, sig) = dwt ls sig

benchIdwt :: (Array U DIM1 Double, Array U DIM1 Double) -> Array U DIM1 Double
benchIdwt (ls, sig) = idwt ls sig

dataDwt :: RandomGen g => g -> (Array U DIM1 Double, Array U DIM1 Double)
dataDwt gen = (randomishDoubleArray (Z :. lsSize ) 0 255 seed, 
               randomishDoubleArray (Z :. sigSize) 0 255 seed)
    where
      seed = fst . next $ gen
