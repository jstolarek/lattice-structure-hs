module Signal.Wavelet.EvalBench where

import Signal.Wavelet.Eval


{-# INLINE benchDwt #-}
benchDwt :: ([Double], [Double]) -> [Double]
benchDwt (ls, sig) = dwt ls sig


{-# INLINE benchIdwt #-}
benchIdwt :: ([Double], [Double]) -> [Double]
benchIdwt (ls, sig) = idwt ls sig


dataDwt :: ([Double], [Double]) -> ([Double], [Double])
dataDwt = id
