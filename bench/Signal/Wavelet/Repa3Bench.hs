module Signal.Wavelet.Repa3Bench where

import Data.Array.Repa
import Data.Vector.Unboxed (Vector)
import GHC.Conc.Sync       (numCapabilities)

import Signal.Wavelet.Repa3


{-# INLINE benchLattice #-}
benchLattice :: ((Double, Double), Array U DIM1 Double) -> Array U DIM1 Double
benchLattice (baseOp, sig) = lattice baseOp $ sig


benchDistributeWork :: (Int, Int) -> Vector Int
benchDistributeWork (threads, sigLength) = distributeWork threads sigLength


dataDistributeWork :: ([Double], [Double]) -> (Int, Int)
dataDistributeWork (_, sig) = (numCapabilities, length sig)
