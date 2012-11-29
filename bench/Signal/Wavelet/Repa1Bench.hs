{-# LANGUAGE FlexibleContexts #-}
module Signal.Wavelet.Repa1Bench where

import Control.Arrow   ((&&&))
import Data.Array.Repa

import Signal.Wavelet.Repa1

import Signal.Wavelet.Repa.Common (forceS, forceP)

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
benchLatticeS (baseOp, sig) = forceS . lattice baseOp $ sig


{-# INLINE benchLatticeP #-}
benchLatticeP :: ((Double, Double), Array U DIM1 Double)
              -> Array U DIM1 Double
benchLatticeP (baseOp, sig) = forceP . lattice baseOp $ sig


dataLattice :: ([Double], [Double])
            -> ((Double, Double), Array U DIM1 Double)
dataLattice (ls, sig) = ((sin &&& cos) . head $ ls, 
                          fromListUnboxed (Z :. sigSize) sig)
    where sigSize = length sig


{-# INLINE benchToPairsS #-}
benchToPairsS :: Array U DIM1 Double
              -> Array U DIM1 (Double, Double)
benchToPairsS = forceS . toPairs


{-# INLINE benchToPairsP #-}
benchToPairsP :: Array U DIM1 Double
              -> Array U DIM1 (Double, Double)
benchToPairsP = forceP . toPairs


dataToPairs :: ([Double], [Double]) -> Array U DIM1 Double
dataToPairs (_, sig) = fromListUnboxed (Z :. sigSize) sig
    where sigSize = length sig


{-# INLINE benchFromPairsS #-}
benchFromPairsS :: Array U DIM1 (Double, Double)
                -> Array U DIM1 Double
benchFromPairsS = forceS . fromPairs


{-# INLINE benchFromPairsP #-}
benchFromPairsP :: Array U DIM1 (Double, Double)
                -> Array U DIM1 Double
benchFromPairsP = forceP . fromPairs


dataFromPairs :: ([Double], [Double]) -> Array U DIM1 (Double, Double)
dataFromPairs (_, sig) = forceS . toPairs . fromListUnboxed (Z :. sigSize) $ sig
    where sigSize = length sig


{-# INLINE benchCslS #-}
benchCslS :: Array U DIM1 Double
          -> Array U DIM1 Double
benchCslS = forceS . csl


{-# INLINE benchCslP #-}
benchCslP :: Array U DIM1 Double
          -> Array U DIM1 Double
benchCslP = forceP . csl


{-# INLINE benchCsrS #-}
benchCsrS :: Array U DIM1 Double
          -> Array U DIM1 Double
benchCsrS = forceS . csr


{-# INLINE benchCsrP #-}
benchCsrP :: Array U DIM1 Double
          -> Array U DIM1 Double
benchCsrP = forceP . csr


dataCslCsr :: ([Double], [Double]) -> Array U DIM1 Double
dataCslCsr (_, sig) = fromListUnboxed (Z :. sigSize) sig
    where sigSize = length sig
