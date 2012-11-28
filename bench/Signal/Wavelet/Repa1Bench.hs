{-# LANGUAGE FlexibleContexts #-}
module Signal.Wavelet.Repa1Bench where

import Control.Arrow   ((&&&))
import Data.Array.Repa

import Signal.Wavelet.Repa1


{-# INLINE benchDwtS #-}
benchDwtS :: (Array U DIM1 Double, Array U DIM1 Double) 
          -> Array U DIM1 Double
benchDwtS (ls, sig) = dwtS ls sig


{-# INLINE benchDwtP #-}
benchDwtP :: (Array U DIM1 Double, Array U DIM1 Double) 
          -> Array U DIM1 Double
benchDwtP (ls, sig) = dwtP ls sig


{-# INLINE benchIdwtS #-}
benchIdwtS :: (Array U DIM1 Double, Array U DIM1 Double)
           -> Array U DIM1 Double
benchIdwtS (ls, sig) = idwtS ls sig


{-# INLINE benchIdwtP #-}
benchIdwtP :: (Array U DIM1 Double, Array U DIM1 Double)
           -> Array U DIM1 Double
benchIdwtP (ls, sig) = idwtP ls sig


dataDwt :: ([Double], [Double])
         -> (Array U DIM1 Double, Array U DIM1 Double)
dataDwt (ls, sig) = (fromListUnboxed (Z :. lsSize ) ls, 
                     fromListUnboxed (Z :. sigSize) sig)
    where
      lsSize  = length ls
      sigSize = length sig


{-# INLINE benchLatticeS #-}
benchLatticeS :: ((Double, Double), Array U DIM1 Double)
             -> Array U DIM1 Double
benchLatticeS (baseOp, sig) = latticeS baseOp sig


{-# INLINE benchLatticeP #-}
benchLatticeP :: ((Double, Double), Array U DIM1 Double)
             -> Array U DIM1 Double
benchLatticeP (baseOp, sig) = latticeP baseOp sig


dataLattice :: ([Double], [Double])
            -> ((Double, Double), Array U DIM1 Double)
dataLattice (ls, sig) = ((sin &&& cos) . head $ ls, 
                          fromListUnboxed (Z :. sigSize) sig)
    where sigSize = length sig
