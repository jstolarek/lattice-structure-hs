{-# LANGUAGE FlexibleContexts, ImpredicativeTypes #-}
module Signal.Wavelet.Repa1Bench where

import Control.Arrow        ((&&&))
import Data.Array.Repa
import Data.Array.Repa.Eval (Load)

import Signal.Wavelet.Repa.Common (forceS, forceP)
import Signal.Wavelet.Repa1


{-# INLINE benchDwt #-}
benchDwt :: (Array U DIM1 Double, Array U DIM1 Double) -> Array U DIM1 Double
benchDwt (ls, sig) = dwt ls sig


{-# INLINE benchIdwt #-}
benchIdwt :: (Array U DIM1 Double, Array U DIM1 Double) -> Array U DIM1 Double
benchIdwt (ls, sig) = idwt ls sig


dataDwt :: ([Double], [Double])
        -> (Array U DIM1 Double, Array U DIM1 Double)
dataDwt (ls, sig) = (fromListUnboxed (Z :. lsSize ) ls, 
                     fromListUnboxed (Z :. sigSize) sig)
    where
      lsSize  = length ls
      sigSize = length sig


{-# INLINE benchLattice #-}
benchLattice :: (Source r Double) =>
               (Array D DIM1 Double -> Array U DIM1 Double, 
                (Double, Double), Array r DIM1 Double)
             -> Array U DIM1 Double
benchLattice (force, baseOp, sig) = force . lattice baseOp $ sig


dataLatticeP :: (Load r DIM1 Double)
             => ([Double], [Double])
             -> (Array r DIM1 Double -> Array U DIM1 Double,
                (Double, Double), Array U DIM1 Double)
dataLatticeP (ls, sig) = (forceP, (sin &&& cos) . head $ ls, 
                          fromListUnboxed (Z :. sigSize) sig)
    where sigSize = length sig


dataLatticeS :: (Load r DIM1 Double)
             => ([Double], [Double])
             -> (Array r DIM1 Double -> Array U DIM1 Double,
                (Double, Double), Array U DIM1 Double)
dataLatticeS (ls, sig) = (forceS, (sin &&& cos) . head $ ls, 
                         fromListUnboxed (Z :. sigSize) sig)
    where sigSize = length sig
