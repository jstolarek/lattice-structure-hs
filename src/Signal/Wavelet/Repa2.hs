module Signal.Wavelet.Repa2 where

import Data.Array.Repa        as R
import Data.Array.Repa.Unsafe (unsafeTraverse)

import Signal.Wavelet.Repa.Common


{-# INLINE dwtS #-}
{-# INLINE dwtP #-}
{-# INLINE idwtS #-}
{-# INLINE idwtP #-}
dwtS, dwtP, idwtS, idwtP :: Array U DIM1 Double
                         -> Array U DIM1 Double
                         -> Array U DIM1 Double
dwtS  !angles !signal = dwtWorkerS extendEnd   angles signal
dwtP  !angles !signal = dwtWorkerP extendEnd   angles signal
idwtS !angles !signal = dwtWorkerS extendFront angles signal
idwtP !angles !signal = dwtWorkerP extendFront angles signal


-- See: Note [Higher order functions interfere with fusion] in Repa1.hs
{-# INLINE dwtWorkerS #-}
dwtWorkerS, dwtWorkerP :: (Source r Double)
                       => (Int -> Array r DIM1 Double -> Array D DIM1 Double)
                       -> Array U DIM1 Double
                       -> Array r DIM1 Double
                       -> Array U DIM1 Double
dwtWorkerS extendF !angles !signal = go layers extendedSignal
    where
      !extendedSignal = forceS $ extendF layers signal
      !layers = size . extent $ angles
      {-# INLINE go #-}
      go :: Int -> Array U DIM1 Double -> Array U DIM1 Double
      go !n sig
          | n == 0    = sig
          | n == 1    = forceS . lattice (sin_, cos_) $ sig
          | otherwise = go (n - 1) (forceS . trim . lattice (sin_, cos_) $ sig)
          where !sin_ = sin $ angles `unsafeIndex` (Z :. (layers - n))
                !cos_ = cos $ angles `unsafeIndex` (Z :. (layers - n))


{-# INLINE dwtWorkerP #-}
dwtWorkerP extendF !angles !signal = go layers extendedSignal
    where
      !extendedSignal = forceP $ extendF layers signal
      !layers = size . extent $ angles
      {-# INLINE go #-}
      go :: Int -> Array U DIM1 Double -> Array U DIM1 Double
      go !n !sig
          | n == 0    = sig
          | n == 1    = forceP . lattice (sin_, cos_) $ sig
          | otherwise = go (n - 1) (forceP . trim . lattice (sin_, cos_) $ sig)
          where !sin_ = sin $ angles `unsafeIndex` (Z :. (layers - n))
                !cos_ = cos $ angles `unsafeIndex` (Z :. (layers - n))


{-# INLINE lattice #-}
lattice :: (Shape sh)
        => (Double, Double)
        -> Array U (sh :. Int) Double
        -> Array D (sh :. Int) Double
lattice !(!s, !c) !signal = unsafeTraverse signal id baseOp
    where
      {-# INLINE baseOp #-}
      baseOp f !(sh :. i)
             | even i    = let x = f (sh :. i    )
                               y = f (sh :. i + 1)
                           in x * c + y * s
             | otherwise = let x = f (sh :. i - 1)
                               y = f (sh :. i    )
                           in x * s - y * c


{-# INLINE extendFront #-}
extendFront :: (Source r Double, Shape sh)
            => Int
            -> Array r (sh :. Int) Double
            -> Array D (sh :. Int) Double
extendFront !layers !signal = go (delay signal) initExt initSigSize
    where !initExt     = 2 * layers - 2 :: Int
          !initSigSize = size . extent $ signal :: Int
          !(sh :. _)   = extent signal
          {-# INLINE go #-}
          go !sig !ln !sigSize
              | extSize <= 0   = sig
              | otherwise      = go extSignal (ln - extSize) (sigSize + extSize)
              where !extSize   = min sigSize ln :: Int
                    !extSignal = extract (sh :. sigSize - extSize)
                                         (sh :. extSize) sig R.++ sig


{-# INLINE extendEnd #-}
extendEnd :: (Source r Double, Shape sh)
          => Int
          -> Array r (sh :. Int) Double
          -> Array D (sh :. Int) Double
extendEnd !layers !signal = go (delay signal) initExt initSigSize
    where !initExt     = 2 * layers - 2 :: Int
          !initSigSize = size . extent $ signal :: Int
          !(sh :. _)   = extent signal
          {-# INLINE go #-}
          go !sig !ln !sigSize
              | extSize <= 0   = sig
              | otherwise      = go extSignal (ln - extSize) (sigSize + extSize)
              where !extSize   = min sigSize ln :: Int
                    !extSignal = sig R.++ extract (sh :. 0) (sh :. extSize) sig


{-# INLINE trim #-}
trim :: (Source r Double, Shape sh)
     => Array r (sh :. Int) Double
     -> Array D (sh :. Int) Double
trim !signal = unsafeTraverse signal trimExtent mapElems
    where
      {-# INLINE trimExtent #-}
      trimExtent !(sh :. i) =   (sh :. max (i - 2) 0)
      {-# INLINE mapElems #-}
      mapElems f !(sh :. i) = f (sh :. (i + 1))
