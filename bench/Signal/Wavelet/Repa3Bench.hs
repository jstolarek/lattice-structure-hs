{-# LANGUAGE FlexibleContexts #-}
module Signal.Wavelet.Repa3Bench where

import Data.Array.Repa

import Signal.Wavelet.Repa3


{-# INLINE benchLattice #-}
benchLattice :: (Array L DIM1 Double -> Array U DIM1 Double,
                 (Double, Double), Array U DIM1 Double)
             -> Array U DIM1 Double
benchLattice (force, baseOp, sig) = force . lattice baseOp $ sig
