{-# LANGUAGE BangPatterns #-}

module Signal.Wavelet.Vector1 where

import Control.Arrow ((&&&))
import Control.Monad.ST (runST)
import qualified Data.Vector.Generic         as VG
import qualified Data.Vector.Generic.Mutable as VM
import           Data.Vector.Unboxed         as U


{-# INLINE dwt #-}
dwt :: Vector Double
    -> Vector Double 
    -> Vector Double
dwt angles signal = dwtWorker 0 layersCount layerModifier angles signal
    where
      layerModifier = 0
      layersCount   = U.length angles


{-# INLINE idwt #-}
idwt :: Vector Double
     -> Vector Double 
     -> Vector Double
idwt angles signal = dwtWorker 0 layersCount layerModifier angles signal
    where
      layerModifier | even layersCount = 1
                    | otherwise        = 0
      layersCount = U.length angles


{-# INLINE dwtWorker #-}
dwtWorker :: Int 
          -> Int
          -> Int
          -> Vector Double 
          -> Vector Double 
          -> Vector Double
dwtWorker !currentLayer !layersCount !layerModifier angles signal
    | currentLayer == layersCount = signal
    | otherwise = dwtWorker nextLayer layersCount newModifier angles newSignal
    where
      nextLayer    = currentLayer + 1
      newModifier  = 1 - layerModifier
      (sin_, cos_) = (sin &&& cos) $ angles `unsafeIndex` currentLayer
      newSignal    = lattice layerModifier (sin_, cos_) signal


{-# INLINE lattice #-}
lattice :: Int
        -> (Double, Double)
        -> Vector Double
        -> Vector Double
lattice !layerModifier !(!sin_, !cos_) signal = runST $ do
    outVec <- VM.unsafeNew sigSize
    inVec  <- VG.unsafeThaw signal
    fill inVec outVec layerModifier
    VG.unsafeFreeze outVec
        where
          !sigSpan = sigSize - 2 * layerModifier
          !sigSize = U.length signal
          fill inV outV !i
              | i < sigSpan = do
                  x <- VM.unsafeRead inV i
                  y <- VM.unsafeRead inV (i + 1)
                  VM.unsafeWrite outV i       (x * cos_ + y * sin_)
                  VM.unsafeWrite outV (i + 1) (x * sin_ - y * cos_)
                  fill inV outV (i + 2)
              | otherwise = 
                  if (layerModifier == 0 || sigSize == 0) 
                  then return () 
                  else do
                    x <- VM.unsafeRead inV (sigSize - 1)
                    y <- VM.unsafeRead inV 0
                    VM.unsafeWrite outV (sigSize - 1) (x * cos_ + y * sin_)
                    VM.unsafeWrite outV 0             (x * sin_ - y * cos_)
