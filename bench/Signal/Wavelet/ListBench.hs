module Signal.Wavelet.ListBench where

import Signal.Wavelet.List
import System.Random


{-# INLINE benchDwt #-}
benchDwt :: (LS, [Double]) -> [Double]
benchDwt (ls, sig) = dwt ls sig


{-# INLINE benchIdwt #-}
benchIdwt :: (LS, [Double]) -> [Double]
benchIdwt (ls, sig) = idwt ls sig


dataDwt :: RandomGen g => g -> Int -> Int -> (LS, [Double])
dataDwt gen lsSize sigSize = 
    (take lsSize $ randoms gen, take sigSize $ randoms gen)
