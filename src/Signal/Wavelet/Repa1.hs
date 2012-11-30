{-# LANGUAGE FlexibleContexts, BangPatterns #-}
module Signal.Wavelet.Repa1 where

import Control.Arrow          ((&&&))
import Data.Array.Repa        as R
import Data.Array.Repa.Unsafe (unsafeBackpermute, unsafeTraverse)

import Signal.Wavelet.Repa.Common (forceS, forceP)


{-# INLINE dwtS #-}
{-# INLINE dwtP #-}
{-# INLINE idwtS #-}
{-# INLINE idwtP #-}
dwtS, dwtP, idwtS, idwtP :: Array U DIM1 Double
                         -> Array U DIM1 Double 
                         -> Array U DIM1 Double
dwtS  angles signal = dwtWorkerS csl angles signal
dwtP  angles signal = dwtWorkerP csl angles signal
idwtS angles signal = dwtWorkerS csr angles signal
idwtP angles signal = dwtWorkerP csr angles signal


-- See: Note [Higher order functions interfere with fusion]
{-# INLINE dwtWorkerS #-}
dwtWorkerS, dwtWorkerP :: (Array U DIM1 Double -> Array D DIM1 Double)
                       -> Array U DIM1 Double 
                       -> Array U DIM1 Double 
                       -> Array U DIM1 Double
dwtWorkerS cs angles signal = go layers signal
    where
      go :: Int -> Array U DIM1 Double -> Array U DIM1 Double
      go  0 !sig      = sig
      go  1 !sig      = doLayer 1 sig
      go !n !sig      = go (n-1) (forceS . cs $ doLayer n sig)
      {-# INLINE doLayer #-}
      doLayer !n !sig = forceS . lattice 
                        (weights `unsafeIndex` (Z :. (layers - n))) $ sig
      weights         = a2w angles
      layers          = size . extent $ angles


{-# INLINE dwtWorkerP #-}
dwtWorkerP cs angles signal = go layers signal
    where
      go :: Int -> Array U DIM1 Double -> Array U DIM1 Double
      go  0 !sig      = sig
      go  1 !sig      = doLayer 1 sig
      go !n !sig      = go (n-1) (forceP . cs $ doLayer n sig)
      {-# INLINE doLayer #-}
      doLayer !n !sig = forceP . lattice 
                        (weights `unsafeIndex` (Z :. (layers - n))) $ sig
      weights         = a2w angles
      layers          = size . extent $ angles


{-# INLINE lattice #-}
lattice :: (Source r Double) 
        => (Double, Double) 
        -> Array r DIM1 Double
        -> Array D DIM1 Double
lattice !(!s, !c) xs = fromPairs . R.map baseOp . toPairs $ xs
    where
      baseOp !(!x1, !x2) = (x1 * c + x2 * s,  x1 * s - x2 * c)


{-# INLINE toPairs #-}
toPairs :: (Source r Double) 
        => Array r DIM1 Double 
        -> Array D DIM1 (Double, Double)
toPairs xs = unsafeTraverse xs twiceShorter wrapPairs
    where
      {-# INLINE twiceShorter #-}
      twiceShorter (Z :. s) = Z :. s `quot` 2
      {-# INLINE wrapPairs #-}
      wrapPairs f  (Z :. i) = (f ( Z :. 2 * i ), f ( Z :. 2 * i + 1))


{-# INLINE fromPairs #-}
fromPairs :: (Source r (Double,Double))
          => Array r DIM1 (Double, Double)
          -> Array D DIM1 Double
fromPairs xs = unsafeTraverse xs twiceLonger unwrapPairs
    where
      {-# INLINE twiceLonger #-}
      twiceLonger (Z :. s) = Z :. 2 * s
      {-# INLINE unwrapPairs #-}
      unwrapPairs f (Z :. i) 
                      | even i    = fst . f $ ( Z :. i `quot` 2)
                      | otherwise = snd . f $ ( Z :. i `quot` 2)


{-# INLINE a2w #-}
a2w :: (Source r Double) 
    => Array r DIM1 Double 
    -> Array D DIM1 (Double, Double)
a2w = R.map (sin &&& cos)


{-# INLINE csl #-}
csl :: (Source r Double)
    => Array r DIM1 Double 
    -> Array D DIM1 Double
csl xs = unsafeBackpermute ext shift xs
    where
      shift (Z :. i) = if i /= (sh - 1) then Z :. (i + 1) else Z :. 0
      ext = extent xs
      !sh = size ext


{-# INLINE csr #-}
csr :: (Source r Double)
    => Array r DIM1 Double 
    -> Array D DIM1 Double
csr xs = unsafeBackpermute ext shift xs
    where
      shift (Z :. 0) = Z :. (sh - 1)
      shift (Z :. i) = Z :. ( i - 1)
      ext = extent xs
      !sh = size ext


{-# INLINE cslN #-}
cslN :: (Source r Double) 
     => Int
     -> Array r DIM1 Double 
     -> Array D DIM1 Double
cslN !m xs = unsafeBackpermute ext shift xs
    where
      !n | sh == 0   = 0 -- See Note [Preventing division by 0]
         | otherwise = m `mod` sh :: Int
      shift (Z :. i) = if i < (sh - n) 
                       then Z :. (i + n) 
                       else Z :. (i + n - sh)
      ext = extent xs
      !sh = size ext


{-# INLINE csrN #-}
csrN :: (Source r Double)
     => Int
     -> Array r DIM1 Double 
     -> Array D DIM1 Double
csrN !m xs = unsafeBackpermute ext shift xs
    where
      !n | sh == 0   = 0 -- See Note [Preventing division by 0]
         | otherwise = m `mod` sh :: Int
      shift (Z :. i) = if i >= n
                       then Z :. (i - n) 
                       else Z :. (i - n + sh)
      ext = extent xs
      !sh = size ext

{-

Note [Higher order functions interfere with fusion]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Creating two specialized workers that differ only with forceS/forceP function
is a lot of boilerplate. It would be better to implement dwtWorker as a higher
order function and pass either forceS or forceP as a parameter. Such approach 
however interferes with fusion for unknown reason. I noticed however that when 
I passed forceS/forceP to dwt, but ignored it and passed concrete force to
dwtWorker like this:

dwt force angles signal = dwtWorker forceS csl angles signal

then fusion worked.


Note [Preventing division by 0]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Guards are needed because n is strict. If it were lazy, backpermute would not
perform computations for empty array and thus n wouldn't be calculated. However
when n is forced with a bang pattern it is always evaluated and hence it will
cause division by 0 error for `mod` operator.

-}
