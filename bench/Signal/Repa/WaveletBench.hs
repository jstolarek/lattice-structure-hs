module Signal.Repa.WaveletBench where

import Data.Array.Repa
import Data.Array.Repa.Algorithms.Randomish
import System.Random

import Signal.Repa.Wavelet

benchDwt :: (Array U DIM1 Double, Array D DIM1 Double) -> Array U DIM1 Double
benchDwt (ls, sig) = dwtR ls sig

dataDwt :: RandomGen g => g -> (Array U DIM1 Double, Array D DIM1 Double)
dataDwt gen = (randomishDoubleArray (Z :. 8   ) 0 255 seed, 
       delay $ randomishDoubleArray (Z :. 8192) 0 255 seed)
    where
      seed = fst . next $ gen