module Signal.WaveletBench where

import System.Random

import Signal.Wavelet

benchDwt :: (LS, [Double]) -> [Double]
benchDwt (ls, sig) = dwt ls sig

dataDwt :: RandomGen g => g -> (LS, [Double])
dataDwt gen = (take 8 $ randoms gen, take 16384 $ randoms gen)
