module Signal.WaveletBench where

import Criterion
import System.Random

import Signal.Wavelet

benchDwt :: (LS, [Double]) -> [Double]
benchDwt (ls, sig) = dwt ls sig

dataDwt :: RandomGen g => g -> (LS, [Double])
dataDwt gen = (take 5 $ randoms gen, take 1024 $ randoms gen)
