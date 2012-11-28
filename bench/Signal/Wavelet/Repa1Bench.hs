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


{-# INLINE benchToPairsS #-}
benchToPairsS :: Array U DIM1 Double
              -> Array U DIM1 (Double, Double)
benchToPairsS sig = toPairsS sig


{-# INLINE benchToPairsP #-}
benchToPairsP :: Array U DIM1 Double
              -> Array U DIM1 (Double, Double)
benchToPairsP sig = toPairsP sig


dataToPairs :: ([Double], [Double]) -> Array U DIM1 Double
dataToPairs (_, sig) = fromListUnboxed (Z :. sigSize) sig
    where sigSize = length sig


{-# INLINE benchFromPairsS #-}
benchFromPairsS :: Array U DIM1 (Double, Double)
                -> Array U DIM1 Double
benchFromPairsS sig = fromPairsS sig


{-# INLINE benchFromPairsP #-}
benchFromPairsP :: Array U DIM1 (Double, Double)
                -> Array U DIM1 Double
benchFromPairsP sig = fromPairsP sig


dataFromPairs :: ([Double], [Double]) -> Array U DIM1 (Double, Double)
dataFromPairs (_, sig) = toPairsS . fromListUnboxed (Z :. sigSize) $ sig
    where sigSize = length sig
