{-# LANGUAGE BangPatterns #-}

module Signal.Wavelet.Repa2 where

import Control.Arrow ((&&&))
--import Control.Monad.ST (runST)
import Data.Array.Repa as R
--import Data.Array.Repa.Unsafe
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VM

import System.IO.Unsafe

{-# INLINE dwt #-}
dwt :: Array U DIM1 Double
    -> Array U DIM1 Double 
    -> Array U DIM1 Double
dwt !angles !signal = dwtWorker 0 lSize lm angles signal
    where
      lm    = 0
      lSize = size . extent $ angles


{-# INLINE idwt #-}
idwt :: Array U DIM1 Double 
     -> Array U DIM1 Double 
     -> Array U DIM1 Double
idwt !angles !signal = dwtWorker 0 lSize lm angles signal
    where
      lm | even lSize   = 1
         | otherwise = 0
      lSize = size . extent $ angles


{-# INLINE dwtWorker #-}
dwtWorker :: Int 
          -> Int
          -> Int
          -> Array U DIM1 Double 
          -> Array U DIM1 Double 
          -> Array U DIM1 Double
dwtWorker cl lSize lm !angles !signal
    | cl == lSize  = signal -- można jedną iterację mniej?
    | otherwise    = dwtWorker (cl + 1) lSize (1 - lm) angles newSignal
    where
      (sin_, cos_) = (sin &&& cos) $ angles ! (Z :. cl)
      sSize        = size . extent $ signal
      newSignal    = lattice sSize lm signal sin_ cos_


{-# INLINE lattice #-}
lattice :: Int
        -> Int
        -> Array U DIM1 Double
        -> Double
        -> Double
        -> Array U DIM1 Double
lattice sSize lm signal sin_ cos_ = fromUnboxed (Z :. sSize) . unsafePerformIO $ do
    vec <- VM.unsafeNew sSize
    sv  <- VG.unsafeThaw . toUnboxed $ signal
    fill sv vec lm (sSize - 2 * lm)
    VG.unsafeFreeze vec
        where
          fill sv v i stopS
              | i < stopS = do
                 x <- VM.unsafeRead sv i
                 y <- VM.unsafeRead sv (i+1)
                 VM.unsafeWrite v i     (x * cos_ + y * sin_)
                 VM.unsafeWrite v (i+1) (x * sin_ - y * cos_)
                 fill sv v (i + 2) stopS
              | otherwise = if (lm == 0 || sSize == 0) then return () else do
                 x <- VM.unsafeRead sv (sSize - 1)
                 y <- VM.unsafeRead sv 0
                 VM.unsafeWrite v (sSize - 1) (x * cos_ + y * sin_)
                 VM.unsafeWrite v 0           (x * sin_ - y * cos_)
                 return ()
