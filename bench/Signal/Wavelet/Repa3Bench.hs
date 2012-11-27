module Signal.Wavelet.Repa3Bench where

import Data.Array.Repa

import Signal.Wavelet.Repa3


{-# INLINE benchLattice #-}
benchLattice :: ((Double, Double), Array U DIM1 Double) -> Array U DIM1 Double
benchLattice (baseOp, sig) = lattice baseOp $ sig

