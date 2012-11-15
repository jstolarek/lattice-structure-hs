{-# LANGUAGE FlexibleContexts, BangPatterns #-}
module Signal.Wavelet.Repa2 where

import Data.Array.Repa as R
import Data.Array.Repa.Unsafe
import Signal.Wavelet.Repa.Common


{-# INLINE dwt #-}
dwt :: Array U DIM1 Double
    -> Array U DIM1 Double 
    -> Array U DIM1 Double
dwt angles signal = dwtWorker angles extendedSignal
    where 
      extendedSignal = forceP $ extendEnd layers signal
      layers         = size . extent $ angles


{-# INLINE idwt #-}
idwt :: Array U DIM1 Double
     -> Array U DIM1 Double 
     -> Array U DIM1 Double
idwt angles signal = dwtWorker angles extendedSignal
    where
      extendedSignal = forceP $ extendFront layers signal
      layers         = size . extent $ angles


{-# INLINE dwtWorker #-}
dwtWorker :: Array U DIM1 Double
          -> Array U DIM1 Double 
          -> Array U DIM1 Double
dwtWorker angles signal = go layers signal
    where
      !layers = size . extent $ angles
      go :: Int -> Array U DIM1 Double -> Array U DIM1 Double
      go !n sig  
          | n == 0    = sig
          | n == 1    = forceP . lattice (sin_, cos_) $ sig
          | otherwise = go (n - 1) (forceP . trim . lattice (sin_, cos_) $ sig)
          where sin_  = sin $ angles `unsafeIndex` (Z :. (layers - n))
                cos_  = cos $ angles `unsafeIndex` (Z :. (layers - n))


{-# INLINE lattice #-}
lattice :: (Double, Double) 
        -> Array U DIM1 Double
        -> Array D DIM1 Double
lattice !(!s, !c) !signal = unsafeTraverse signal id baseOp
    where
      baseOp f (Z :. i) 
             | even i    = let x = f (Z :. i    )
                               y = f (Z :. i + 1)
                           in x * c + y * s
             | otherwise = let x = f (Z :. i - 1)
                               y = f (Z :. i    )
                           in x * s - y * c


{-# INLINE extendFront #-}
extendFront :: Int
            -> Array U DIM1 Double
            -> Array D DIM1 Double
extendFront !layers signal = go (delay signal) initExt initSigSize
    where !initExt     = 2 * layers - 2 :: Int
          !initSigSize = size . extent $ signal :: Int
          go sig !ln !sigSize
              | extSize <= 0   = sig
              | otherwise      = go extSignal (ln - extSize) (sigSize + extSize)
              where !extSize   = min sigSize ln :: Int
                    !extSignal = extract (Z :. sigSize - extSize) 
                                         (Z :. extSize) sig R.++ sig


{-# INLINE extendEnd #-}
extendEnd :: Int
          -> Array U DIM1 Double
          -> Array D DIM1 Double
extendEnd !layers signal = go (delay signal) initExt initSigSize
    where !initExt     = 2 * layers - 2 :: Int
          !initSigSize = size . extent $ signal :: Int
          go sig !ln !sigSize
              | extSize <= 0   = sig
              | otherwise      = go extSignal (ln - extSize) (sigSize + extSize)
              where !extSize   = min sigSize ln :: Int
                    !extSignal = sig R.++ extract (Z :. 0) (Z :. extSize) sig


{-# INLINE trim #-}
trim :: Array D DIM1 Double
     -> Array D DIM1 Double
trim signal = unsafeTraverse signal trimExtent mapElems
    where
      {-# INLINE trimExtent #-}
      trimExtent (Z :. i) =   (Z :. max (i - 2) 0)
      {-# INLINE mapElems #-}
      mapElems f (Z :. i) = f (Z :. (i + 1))
