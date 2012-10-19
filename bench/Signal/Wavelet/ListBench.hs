module Signal.Wavelet.ListBench where

import System.Random

import Signal.Wavelet.List

benchDwt :: (LS, [Double]) -> [Double]
benchDwt (ls, sig) = dwt ls sig

benchIdwt :: (LS, [Double]) -> [Double]
benchIdwt (ls, sig) = idwt ls sig

dataDwt :: RandomGen g => g -> (LS, [Double])
dataDwt gen = (take 8 $ randoms gen, take 8192 $ randoms gen)
