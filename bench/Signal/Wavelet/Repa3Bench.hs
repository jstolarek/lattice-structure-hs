{-# LANGUAGE FlexibleContexts #-}
module Signal.Wavelet.Repa3Bench where

import Data.Array.Repa

import Signal.Wavelet.Repa3


{-# INLINE benchLatticeS #-}
benchLatticeS :: ((Double, Double), Array U DIM1 Double)
             -> Array U DIM1 Double
benchLatticeS (baseOp, sig) = latticeS baseOp $ sig


{-# INLINE benchLatticeP #-}
benchLatticeP :: ((Double, Double), Array U DIM1 Double)
             -> Array U DIM1 Double
benchLatticeP (baseOp, sig) = latticeP baseOp $ sig
