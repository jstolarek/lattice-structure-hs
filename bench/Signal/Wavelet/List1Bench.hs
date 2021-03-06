module Signal.Wavelet.List1Bench where

import System.Random (RandomGen, randoms)

import Signal.Wavelet.List1


{-# INLINE benchDwt #-}
benchDwt :: ([Double], [Double]) -> [Double]
benchDwt (ls, sig) = dwt ls sig


{-# INLINE benchIdwt #-}
benchIdwt :: ([Double], [Double]) -> [Double]
benchIdwt (ls, sig) = idwt ls sig


dataDwt :: RandomGen g => g -> Int -> Int -> ([Double], [Double])
dataDwt gen lsSize sigSize =
    (take lsSize $ randoms gen, take sigSize $ randoms gen)
