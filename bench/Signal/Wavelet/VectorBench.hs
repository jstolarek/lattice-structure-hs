module Signal.Wavelet.VectorBench where

import Data.Vector.Unboxed
import Signal.Wavelet.Vector


{-# INLINE benchDwt #-}
benchDwt :: (Vector Double, Vector Double) -> Vector Double
benchDwt (ls, sig) = dwt ls sig


{-# INLINE benchIdwt #-}
benchIdwt :: (Vector Double, Vector Double) -> Vector Double
benchIdwt (ls, sig) = idwt ls sig


dataDwt :: ([Double], [Double])
        -> (Vector Double, Vector Double)
dataDwt (ls, sig) = (fromList ls, fromList sig)
