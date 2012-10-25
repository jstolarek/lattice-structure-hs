{-# LANGUAGE FlexibleContexts, BangPatterns #-}

module Signal.Wavelet.Repa1 where

import Control.Arrow
import Data.Array.Repa as R
import Signal.Wavelet.Repa.Common


{-# INLINE dwt #-}
dwt :: (Source r Double) 
    => Array r DIM1 Double 
    -> Array U DIM1 Double 
    -> Array U DIM1 Double
dwt !angles !signal = dwtWorker csl angles signal


{-# INLINE idwt #-}
idwt :: (Source r Double) 
     => Array r DIM1 Double 
     -> Array U DIM1 Double 
     -> Array U DIM1 Double
idwt !angles !signal = dwtWorker csr angles signal


{-# INLINE dwtWorker #-}
dwtWorker :: (Source r Double) 
          => (Array U DIM1 Double -> Array D DIM1 Double)
          -> Array r DIM1 Double 
          -> Array U DIM1 Double 
          -> Array U DIM1 Double
dwtWorker cs !angles !signal = go layers signal
    where
      go  0 !sig      = sig
      go  1 !sig      = doLayer sig 1
      go !n !sig      = go (n-1) (forceS . cs $ doLayer sig n)
      {-# INLINE doLayer #-}
      doLayer !sig !n = forceP . lattice (weights ! (Z :. (layers - n))) $ sig
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
      baseOp (!x1, !x2) = (x1 * c + x2 * s,  x1 * s - x2 * c )


{-# INLINE toPairs #-}
toPairs :: (Source r Double) 
        => Array r DIM1 Double 
        -> Array D DIM1 (Double, Double)
toPairs !xs = traverse xs twiceShorter wrapPairs
    where
      twiceShorter (Z :. s) = Z :. s `div` 2
      wrapPairs f  (Z :. i) = ( f ( Z :. 2 * i ), f ( Z :. 2 * i + 1))


{-# INLINE fromPairs #-}
fromPairs :: (Source r (Double,Double))
          => Array r DIM1 (Double, Double)
          -> Array D DIM1 Double
fromPairs !xs = traverse xs twiceLonger unwrapPairs
    where
      twiceLonger   (Z :. s) = Z :. 2 * s
      unwrapPairs f (Z :. i) 
                      | even (i `mod` 2) = fst .f $ ( Z :. i `div` 2)
                      | otherwise        = snd .f $ ( Z :. i `div` 2)


{-# INLINE a2w #-}
a2w :: (Source r Double) 
       => Array r DIM1 Double 
       -> Array D DIM1 (Double, Double)
a2w = R.map (sin &&& cos)


{-# INLINE inv #-}
inv :: (Source r Double)
    =>  Array r DIM1 Double 
    -> Array D DIM1 Double
inv !xs = backpermute ext reversedIndex xs
    where
      reversedIndex (Z :. i) = Z :. (sh - i - 1)
      ext = extent xs
      sh = size ext


{-# INLINE csl #-}
csl :: Array U DIM1 Double 
    -> Array D DIM1 Double
csl !xs = backpermute ext shift xs
    where
      shift (Z :. i) = if i /= (sh - 1) then Z :. (i + 1) else Z :. 0
      ext = extent xs
      sh = size ext


{-# INLINE csr #-}
csr :: Array U DIM1 Double 
    -> Array D DIM1 Double
csr xs = backpermute ext shift xs
    where
      shift (Z :. 0) = Z :. (sh-1)
      shift (Z :. i) = Z :. (i-1)
      ext = extent xs
      sh = size ext
