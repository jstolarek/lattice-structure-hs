module Signal.Repa.WaveletBench where

import Data.Array.Repa
import Data.Array.Repa.Algorithms.Randomish
import System.Random

import Signal.Repa.Wavelet

benchDwt :: (Array U DIM1 Double, Array U DIM1 Double) -> Array U DIM1 Double
benchDwt (ls, sig) = computeS $ dwtR ls sig

dataDwt :: RandomGen g => g -> (Array U DIM1 Double, Array U DIM1 Double)
-- dataDwt gen = (fromListUnboxed (Z :. 6   ) $ take 6    $ randoms gen, 
--        delay $ fromListUnboxed (Z :. 2048) $ take 2048 $ randoms gen)
dataDwt gen = (randomishDoubleArray (Z :. 6) 0 255 seed, randomishDoubleArray (Z :. 2048) 0 255 seed)
    where
      seed = fst . next $ gen