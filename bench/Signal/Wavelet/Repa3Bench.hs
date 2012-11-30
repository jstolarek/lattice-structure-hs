{-# LANGUAGE FlexibleContexts #-}
module Signal.Wavelet.Repa3Bench where

import Data.Array.Repa

import Signal.Wavelet.Repa3
import Signal.Wavelet.Repa.Common (forceS, forceP)


{-# INLINE benchLatticeS #-}
benchLatticeS :: ((Double, Double), Array U DIM1 Double)
              -> Array U DIM1 Double
benchLatticeS = forceS . (uncurry lattice)


{-# INLINE benchLatticeP #-}
benchLatticeP :: ((Double, Double), Array U DIM1 Double)
              -> Array U DIM1 Double
benchLatticeP = forceP . (uncurry lattice)
