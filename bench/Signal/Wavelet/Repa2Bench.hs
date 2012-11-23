module Signal.Wavelet.Repa2Bench where

import Control.Arrow ((&&&))
import Data.Array.Repa
import Signal.Wavelet.Repa2
import Signal.Wavelet.Repa.Common


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


dataDwt :: ([Double], [Double])
        -> (Array U DIM1 Double, Array U DIM1 Double)
dataDwt (ls, sig) = (fromListUnboxed (Z :. lsSize ) ls, 
                     fromListUnboxed (Z :. sigSize) sig)
    where
      lsSize  = length ls
      sigSize = length sig


dataLattice :: ([Double], [Double])
            -> ((Double, Double), Array U DIM1 Double)
dataLattice (ls, sig) = ((sin &&& cos) . head $ ls, 
                         fromListUnboxed (Z :. sigSize) sig)
    where sigSize = length sig


dataExtend :: ([Double], [Double])
           -> (Int, Array U DIM1 Double)
dataExtend (ls, sig) = (length ls, fromListUnboxed (Z :. sigSize) sig)
    where sigSize = length sig
