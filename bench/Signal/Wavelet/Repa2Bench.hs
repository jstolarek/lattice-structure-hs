module Signal.Wavelet.Repa2Bench where

import Data.Array.Repa

import Signal.Wavelet.Repa.Common (forceS)
import Signal.Wavelet.Repa2


{-# INLINE benchDwt #-}
benchDwt :: (Array U DIM1 Double, Array U DIM1 Double) -> Array U DIM1 Double
benchDwt (ls, sig) = dwt ls sig


{-# INLINE benchIdwt #-}
benchIdwt :: (Array U DIM1 Double, Array U DIM1 Double) -> Array U DIM1 Double
benchIdwt (ls, sig) = idwt ls sig


{-# INLINE benchLattice #-}
benchLattice :: ((Double, Double), Array U DIM1 Double) -> Array U DIM1 Double
benchLattice (baseOp, sig) = forceS . lattice baseOp $ sig


{-# INLINE benchExtendFront #-}
benchExtendFront :: (Int, Array U DIM1 Double) -> Array U DIM1 Double
benchExtendFront (ls, sig) = forceS . extendFront ls $ sig


{-# INLINE benchExtendEnd #-}
benchExtendEnd :: (Int, Array U DIM1 Double) -> Array U DIM1 Double
benchExtendEnd (ls, sig) = forceS . extendEnd ls $ sig


dataExtend :: ([Double], [Double])
           -> (Int, Array U DIM1 Double)
dataExtend (ls, sig) = (length ls, fromListUnboxed (Z :. sigSize) sig)
    where sigSize = length sig
