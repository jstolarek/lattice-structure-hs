module Signal.Wavelet.Repa3Bench where

import Control.Arrow   ((&&&))
import Data.Array.Repa

import Signal.Wavelet.Repa3
import Signal.Wavelet.Repa.Common (forceS, forceP)

{-# INLINE benchDwtS #-}
benchDwtS :: (Array U DIM1 Double, Array U DIM1 Double) -> Array U DIM1 Double
benchDwtS = uncurry dwtS


{-# INLINE benchDwtP #-}
benchDwtP :: (Array U DIM1 Double, Array U DIM1 Double) -> Array U DIM1 Double
benchDwtP = uncurry dwtP


{-# INLINE benchIdwtS #-}
benchIdwtS :: (Array U DIM1 Double, Array U DIM1 Double) -> Array U DIM1 Double
benchIdwtS = uncurry idwtS


{-# INLINE benchIdwtP #-}
benchIdwtP :: (Array U DIM1 Double, Array U DIM1 Double) -> Array U DIM1 Double
benchIdwtP = uncurry idwtP


{-# INLINE benchLatticeS #-}
benchLatticeS :: (Int, (Double, Double), Array U DIM1 Double)
              -> Array U DIM1 Double
benchLatticeS (lm, baseOp, sig) = forceS . lattice lm baseOp $ sig


{-# INLINE benchLatticeP #-}
benchLatticeP :: (Int, (Double, Double), Array U DIM1 Double)
              -> Array U DIM1 Double
benchLatticeP (lm, baseOp, sig) = forceP . lattice lm baseOp $ sig


dataLattice :: ([Double], [Double])
            -> (Int, (Double, Double), Array U DIM1 Double)
dataLattice (ls, sig) = (1, (sin &&& cos) . Prelude.head $ ls, rSig)
    where rSig = fromListUnboxed (Z :. sigL) sig
          sigL = length sig
