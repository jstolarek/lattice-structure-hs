{-# LANGUAGE FlexibleContexts #-}
module Signal.Wavelet.Repa2Bench where

import Data.Array.Repa

import Signal.Wavelet.Repa2


{-# INLINE benchDwtS #-}
benchDwtS :: (Array U DIM1 Double, Array U DIM1 Double) -> Array U DIM1 Double
benchDwtS (ls, sig) = dwtS ls sig


{-# INLINE benchDwtP #-}
benchDwtP :: (Array U DIM1 Double, Array U DIM1 Double) -> Array U DIM1 Double
benchDwtP (ls, sig) = dwtP ls sig


{-# INLINE benchIdwtS #-}
benchIdwtS :: (Array U DIM1 Double, Array U DIM1 Double) -> Array U DIM1 Double
benchIdwtS (ls, sig) = idwtS ls sig


{-# INLINE benchIdwtP #-}
benchIdwtP :: (Array U DIM1 Double, Array U DIM1 Double) -> Array U DIM1 Double
benchIdwtP (ls, sig) = idwtP ls sig


{-# INLINE benchLatticeS #-}
benchLatticeS :: ((Double, Double), Array U DIM1 Double)
              -> Array U DIM1 Double
benchLatticeS (baseOp, sig) = latticeS baseOp sig


{-# INLINE benchLatticeP #-}
benchLatticeP :: ((Double, Double), Array U DIM1 Double)
              -> Array U DIM1 Double
benchLatticeP (baseOp, sig) = latticeP baseOp sig


{-# INLINE benchExtendFrontS #-}
benchExtendFrontS :: (Int, Array U DIM1 Double) -> Array U DIM1 Double
benchExtendFrontS (ls, sig) = extendFrontS ls $ sig


{-# INLINE benchExtendFrontP #-}
benchExtendFrontP :: (Int, Array U DIM1 Double) -> Array U DIM1 Double
benchExtendFrontP (ls, sig) = extendFrontP ls $ sig


{-# INLINE benchExtendEndS #-}
benchExtendEndS :: (Int, Array U DIM1 Double) -> Array U DIM1 Double
benchExtendEndS (ls, sig) = extendEndS ls $ sig


{-# INLINE benchExtendEndP #-}
benchExtendEndP :: (Int, Array U DIM1 Double) -> Array U DIM1 Double
benchExtendEndP (ls, sig) = extendEndP ls $ sig


dataExtend :: ([Double], [Double])
           -> (Int, Array U DIM1 Double)
dataExtend (ls, sig) = (length ls, fromListUnboxed (Z :. sigSize) sig)
    where sigSize = length sig
