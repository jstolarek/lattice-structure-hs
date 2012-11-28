{-# LANGUAGE FlexibleContexts #-}
module Signal.Wavelet.Repa3Bench where

import Data.Array.Repa
import Data.Vector.Unboxed  (Vector)
import GHC.Conc.Sync        (numCapabilities)

import Signal.Wavelet.Repa3


{-# INLINE benchLattice #-}
benchLattice :: (Array L DIM1 Double -> Array U DIM1 Double,
                 (Double, Double), Array U DIM1 Double)
             -> Array U DIM1 Double
benchLattice (force, baseOp, sig) = force . lattice baseOp $ sig


benchDistributeWork :: (Int, Int) -> Vector Int
benchDistributeWork (threads, sigLength) = distributeWork threads sigLength


dataDistributeWork :: ([Double], [Double]) -> (Int, Int)
dataDistributeWork (_, sig) = (numCapabilities, length sig)
