module Signal.Wavelet.Vector where

import Control.Arrow ((&&&))
import Control.Monad.ST (runST)
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VM
import Data.Vector.Unboxed as U

{-# INLINE dwt #-}
dwt :: Vector Double
    -> Vector Double 
    -> Vector Double
dwt angles signal = dwtWorker 0 lSize lm angles signal
    where
      lm    = 0
      lSize = U.length angles


{-# INLINE idwt #-}
idwt :: Vector Double 
     -> Vector Double 
     -> Vector Double
idwt angles signal = dwtWorker 0 lSize lm angles signal
     where
      lm | even lSize   = 1
         | otherwise = 0
      lSize = U.length angles


{-# INLINE dwtWorker #-}
dwtWorker :: Int 
          -> Int
          -> Int
          -> Vector Double 
          -> Vector Double 
          -> Vector Double
dwtWorker cl lSize lm angles signal
    | cl == lSize  = signal
    | otherwise    = dwtWorker (cl + 1) lSize (1 - lm) angles newSignal
    where
      (sin_, cos_) = (sin &&& cos) $ angles ! cl
      sSize        = U.length signal
      newSignal    = lattice sSize lm signal sin_ cos_


{-# INLINE lattice #-}
lattice :: Int
        -> Int
        -> Vector Double
        -> Double
        -> Double
        -> Vector Double
lattice sSize lm signal sin_ cos_ = runST $ do
    vec <- VM.unsafeNew sSize
    sv  <- VG.unsafeThaw signal
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
