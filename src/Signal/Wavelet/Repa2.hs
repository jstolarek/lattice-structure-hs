{-# LANGUAGE FlexibleContexts, BangPatterns #-}
module Signal.Wavelet.Repa2 where

import Data.Array.Repa        as R
import Data.Array.Repa.Unsafe (unsafeTraverse)

import Signal.Wavelet.Repa.Common


{-# INLINE dwtS #-}
dwtS :: Array U DIM1 Double
     -> Array U DIM1 Double 
     -> Array U DIM1 Double
dwtS angles signal = dwtWorkerS angles extendedSignal
    where 
      extendedSignal = forceS $ extendEnd layers signal
      layers         = size . extent $ angles


{-# INLINE dwtP #-}
dwtP :: Array U DIM1 Double
     -> Array U DIM1 Double 
     -> Array U DIM1 Double
dwtP angles signal = dwtWorkerP angles extendedSignal
    where 
      extendedSignal = forceP $ extendEnd layers signal
      layers         = size . extent $ angles


{-# INLINE idwtS #-}
idwtS :: Array U DIM1 Double
      -> Array U DIM1 Double 
      -> Array U DIM1 Double
idwtS angles signal = dwtWorkerS angles extendedSignal
    where
      extendedSignal = forceS $ extendFront layers signal
      layers         = size . extent $ angles


{-# INLINE idwtP #-}
idwtP :: Array U DIM1 Double
      -> Array U DIM1 Double 
      -> Array U DIM1 Double
idwtP angles signal = dwtWorkerP angles extendedSignal
    where
      extendedSignal = forceP $ extendFront layers signal
      layers         = size . extent $ angles


{-# INLINE dwtWorkerS #-}
dwtWorkerS :: Array U DIM1 Double
           -> Array U DIM1 Double 
           -> Array U DIM1 Double
dwtWorkerS angles signal = go layers signal
    where
      !layers = size . extent $ angles
      go :: Int -> Array U DIM1 Double -> Array U DIM1 Double
      go !n sig
          | n == 0    = sig
          | n == 1    = forceS . lattice (sin_, cos_) $ sig
          | otherwise = go (n - 1) (forceS . trim . lattice (sin_, cos_) $ sig)
          where sin_  = sin $ angles `unsafeIndex` (Z :. (layers - n))
                cos_  = cos $ angles `unsafeIndex` (Z :. (layers - n))


{-# INLINE dwtWorkerP #-}
dwtWorkerP :: Array U DIM1 Double
          -> Array U DIM1 Double 
          -> Array U DIM1 Double
dwtWorkerP angles signal = go layers signal
    where
      !layers = size . extent $ angles
      go :: Int -> Array U DIM1 Double -> Array U DIM1 Double
      go !n sig
          | n == 0    = sig
          | n == 1    = forceP . lattice (sin_, cos_) $ sig
          | otherwise = go (n - 1) (forceP . trim . lattice (sin_, cos_) $ sig)
          where sin_  = sin $ angles `unsafeIndex` (Z :. (layers - n))
                cos_  = cos $ angles `unsafeIndex` (Z :. (layers - n))


{-# INLINE latticeS #-}
latticeS :: (Source r Double) 
        => (Double, Double) 
        -> Array r DIM1 Double
        -> Array U DIM1 Double
latticeS ls xs = forceS . lattice ls $ xs


{-# INLINE latticeP #-}
latticeP :: (Source r Double) 
        => (Double, Double) 
        -> Array r DIM1 Double
        -> Array U DIM1 Double
latticeP ls xs = forceP . lattice ls $ xs


{-# INLINE lattice #-}
lattice :: (Source r Double)
        => (Double, Double) 
        -> Array r DIM1 Double
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


{-# INLINE extendFrontS #-}
extendFrontS :: (Source r Double) 
             => Int
             -> Array r DIM1 Double
             -> Array U DIM1 Double
extendFrontS !layers signal = forceS . extendFront layers $ signal


{-# INLINE extendFrontP #-}
extendFrontP :: (Source r Double) 
             => Int
             -> Array r DIM1 Double
             -> Array U DIM1 Double
extendFrontP !layers signal = forceP . extendFront layers $ signal


{-# INLINE extendFront #-}
extendFront :: (Source r Double) 
            => Int
            -> Array r DIM1 Double
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


{-# INLINE extendEndS #-}
extendEndS :: (Source r Double) 
             => Int
             -> Array r DIM1 Double
             -> Array U DIM1 Double
extendEndS !layers signal = forceS . extendEnd layers $ signal


{-# INLINE extendEndP #-}
extendEndP :: (Source r Double) 
             => Int
             -> Array r DIM1 Double
             -> Array U DIM1 Double
extendEndP !layers signal = forceP . extendEnd layers $ signal


{-# INLINE extendEnd #-}
extendEnd :: (Source r Double) 
          => Int
          -> Array r DIM1 Double
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
trim :: (Source r Double) 
     => Array r DIM1 Double
     -> Array D DIM1 Double
trim signal = unsafeTraverse signal trimExtent mapElems
    where
      {-# INLINE trimExtent #-}
      trimExtent (Z :. i) =   (Z :. max (i - 2) 0)
      {-# INLINE mapElems #-}
      mapElems f (Z :. i) = f (Z :. (i + 1))
