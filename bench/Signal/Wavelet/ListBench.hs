module Signal.Wavelet.ListBench where

import BenchParam
import Signal.Wavelet.List
import System.Random

benchDwt :: (LS, [Double]) -> [Double]
benchDwt (ls, sig) = dwt ls sig

benchIdwt :: (LS, [Double]) -> [Double]
benchIdwt (ls, sig) = idwt ls sig

dataDwt :: RandomGen g => g -> (LS, [Double])
dataDwt gen = (take lsSize $ randoms gen, take sigSize $ randoms gen)
