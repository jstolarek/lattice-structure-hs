module Signal.Wavelet.Eval1Bench where

import Signal.Wavelet.Eval1


{-# INLINE benchDwt #-}
benchDwt :: ([Double], [Double]) -> [Double]
benchDwt (ls, sig) = dwt ls sig


{-# INLINE benchIdwt #-}
benchIdwt :: ([Double], [Double]) -> [Double]
benchIdwt (ls, sig) = idwt ls sig

