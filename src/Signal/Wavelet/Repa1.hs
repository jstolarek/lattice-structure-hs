{-# LANGUAGE FlexibleContexts, TypeOperators, BangPatterns #-}

module Signal.Wavelet.Repa1 where

import Control.Arrow
import Data.Array.Repa as R
import Data.Array.Repa.Unsafe
import Signal.Wavelet.Repa.Common


{-# INLINE dwt #-}
dwt :: Array U DIM1 Double
    -> Array U DIM1 Double 
    -> Array U DIM1 Double
dwt !angles !signal = dwtWorker csl angles signal


{-# INLINE idwt #-}
idwt :: Array U DIM1 Double 
     -> Array U DIM1 Double 
     -> Array U DIM1 Double
idwt !angles !signal = dwtWorker csr angles signal


{-# INLINE dwtWorker #-}
dwtWorker :: (Array U DIM1 Double -> Array D DIM1 Double)
          -> Array U DIM1 Double 
          -> Array U DIM1 Double 
          -> Array U DIM1 Double
dwtWorker cs !angles !signal = go layers signal
    where
      go :: Int -> Array U DIM1 Double -> Array U DIM1 Double
      go  0 !sig      = sig
      go  1 !sig      = doLayer 1 sig
      go !n !sig      = go (n-1) (forceS . cs $ doLayer n sig)
      {-# INLINE doLayer #-}
      doLayer !n !sig = forceP . lattice 
                        (weights `unsafeIndex` (Z :. (layers - n))) $ sig
      {-# INLINE weights #-}
      weights         = a2w angles
      {-# INLINE layers #-}
      layers          = size . extent $ angles


{-# INLINE lattice #-}
lattice :: (Source r Double) 
        => (Double, Double) 
        -> Array r DIM1 Double
        -> Array D DIM1 Double
lattice (!s, !c) !xs = fromPairs . R.map baseOp . toPairs $ xs
    where
      baseOp (!x1, !x2) = (x1 * c + x2 * s,  x1 * s - x2 * c)


{-# INLINE toPairs #-}
toPairs :: (Source r Double) 
        => Array r DIM1 Double 
        -> Array D DIM1 (Double, Double)
toPairs !xs = unsafeTraverse xs twiceShorter wrapPairs
    where
      twiceShorter :: (Z :. Int) -> (Z :. Int)
      twiceShorter !(Z :. s) = Z :. s `quot` 2
      wrapPairs :: ((Z :. Int) -> Double) -> (Z :. Int) -> (Double, Double)
      wrapPairs f  !(Z :. i) = (f ( Z :. 2 * i ), f ( Z :. 2 * i + 1))


{-# INLINE fromPairs #-}
fromPairs :: (Source r (Double,Double))
          => Array r DIM1 (Double, Double)
          -> Array D DIM1 Double
fromPairs !xs = unsafeTraverse xs twiceLonger unwrapPairs
    where
      twiceLonger :: (Z :. Int) -> (Z :. Int)
      twiceLonger !(Z :. s) = Z :. 2 * s
      unwrapPairs :: ((Z :. Int) -> (Double, Double)) -> (Z :. Int) -> Double
      unwrapPairs f !(Z :. i) 
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
csl !xs = unsafeBackpermute ext shift xs
    where
      shift !(Z :. i) = if i /= (sh - 1) then Z :. (i + 1) else Z :. 0
      ext = extent xs
      sh  = size ext


{-# INLINE csr #-}
csr :: (Source r Double)
    => Array r DIM1 Double 
    -> Array D DIM1 Double
csr xs = unsafeBackpermute ext shift xs
    where
      shift !(Z :. 0) = Z :. (sh - 1)
      shift !(Z :. i) = Z :. (i - 1)
      ext = extent xs
      sh  = size ext


{-# INLINE cslN #-}
cslN :: (Source r Double) 
     => Int
     -> Array r DIM1 Double 
     -> Array D DIM1 Double
cslN n !xs = unsafeBackpermute ext shift xs
    where
      shift !(Z :. i) = if i < (sh - n) 
                        then Z :. (i + n) 
                        else Z :. (i + n - sh)
      ext = extent xs
      sh  = size ext


{-# INLINE csrN #-}
csrN :: (Source r Double)
     => Int
     -> Array r DIM1 Double 
     -> Array D DIM1 Double
csrN n xs = unsafeBackpermute ext shift xs
    where
      shift !(Z :. i) = if i >= n
                        then Z :. (i - n) 
                        else Z :. (i - n + sh)
      ext = extent xs
      sh = size ext
