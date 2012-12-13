{-# OPTIONS_GHC -fno-warn-orphans #-}
module Signal.Wavelet.Repa1 where

import Control.Arrow                    ((&&&))
import Data.Array.Repa                  as R
import Data.Array.Repa.Eval             (Elt)
import Data.Array.Repa.Eval.Gang        (theGang, gangIO, gangSize)
import Data.Array.Repa.Eval.Load        (LoadRange(..))
import Data.Array.Repa.Eval.Target      (Target(..))
import Data.Array.Repa.Repr.HintSmall   (S)
import Data.Array.Repa.Repr.Partitioned (P, Range(..))
import Data.Array.Repa.Repr.Undefined   (X)
import Data.Array.Repa.Unsafe           (unsafeBackpermute, unsafeTraverse)
import Debug.Trace                      (traceEventIO)

import Signal.Wavelet.Repa.Common (forceS, forceP)


type PS = P D (P (S D) X)


{-# INLINE dwtS #-}
{-# INLINE dwtP #-}
{-# INLINE idwtS #-}
{-# INLINE idwtP #-}
dwtS, dwtP, idwtS, idwtP :: Array U DIM1 Double
                         -> Array U DIM1 Double 
                         -> Array U DIM1 Double
dwtS  !angles !signal = dwtWorkerS csl angles signal
dwtP  !angles !signal = dwtWorkerP csl angles signal
idwtS !angles !signal = dwtWorkerS csr angles signal
idwtP !angles !signal = dwtWorkerP csr angles signal


-- See: Note [Higher order functions interfere with fusion]
{-# INLINE dwtWorkerS #-}
dwtWorkerS, dwtWorkerP :: (Array D DIM1 Double -> Array D DIM1 Double)
                       -> Array U DIM1 Double 
                       -> Array U DIM1 Double 
                       -> Array U DIM1 Double
dwtWorkerS cs !angles !signal = go layers signal
    where
      go :: Int -> Array U DIM1 Double -> Array U DIM1 Double
      go  0 !sig      = sig
      go  1 !sig      = forceS $ doLayer 1 sig
      go !n !sig      = go (n-1) (forceS . cs $ doLayer n sig)
      {-# INLINE doLayer #-}
      doLayer !n !sig = lattice (weights `unsafeIndex` (Z :.(layers - n))) $ sig
      !weights        = a2w angles
      !layers         = size . extent $ angles


{-# INLINE dwtWorkerP #-}
dwtWorkerP cs !angles !signal = go layers signal
    where
      go :: Int -> Array U DIM1 Double -> Array U DIM1 Double
      go  0 !sig      = sig
      go  1 !sig      = forceP $ doLayer 1 sig
      go !n !sig      = go (n-1) (forceP . cs $ doLayer n sig)
      {-# INLINE doLayer #-}
      doLayer !n !sig = lattice (weights `unsafeIndex` (Z :.(layers - n))) $ sig
      !weights        = a2w angles
      !layers         = size . extent $ angles


{-# INLINE lattice #-}
lattice :: (Source r Double)
        => (Double, Double) 
        -> Array r DIM1 Double
        -> Array D DIM1 Double
lattice !(!s, !c) !xs = fromPairs . R.map baseOp . toPairs $ xs
    where
      baseOp !(!x1, !x2) = (x1 * c + x2 * s,  x1 * s - x2 * c)


{-# INLINE toPairs #-}
toPairs :: (Source r Double) 
        => Array r DIM1 Double 
        -> Array D DIM1 (Double, Double)
toPairs !xs = unsafeTraverse xs twiceShorter wrapPairs
    where
      {-# INLINE twiceShorter #-}
      twiceShorter !(Z :. s) = Z :. s `quot` 2
      {-# INLINE wrapPairs #-}
      wrapPairs f  !(Z :. i) = (f ( Z :. 2 * i ), f ( Z :. 2 * i + 1))


{-# INLINE fromPairs #-}
fromPairs :: (Source r (Double,Double))
          => Array r DIM1 (Double, Double)
          -> Array D DIM1 Double
fromPairs !xs = unsafeTraverse xs twiceLonger unwrapPairs
    where
      {-# INLINE twiceLonger #-}
      twiceLonger !(Z :. s) = Z :. 2 * s
      {-# INLINE unwrapPairs #-}
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
      {-# INLINE shift #-}
      shift !(Z :. i) = if i /= (sh - 1) then Z :. (i + 1) else Z :. 0
      !ext = extent xs
      !sh = size ext


{-# INLINE cslP #-}
cslP :: (Source r Double) 
     => Array r DIM1 Double 
     -> Array PS DIM1 Double
cslP !xs = 
    let !ext   = extent xs
        !sh    = size ext
        !limit = max 0 (sh - 1)
        {-# INLINE innerRange #-}
        innerRange !(Z :. i) = i /= (sh - 1)
        {-# INLINE outerRange #-}
        outerRange = not . innerRange
        inner =          unsafeBackpermute ext (\(Z :. i) -> Z :. (i + 1)) xs
        outer = ASmall $ unsafeBackpermute ext (\_        -> Z :. 0      ) xs
    in APart ext (Range (Z :. 0)     (Z :. limit) innerRange) inner $ 
       APart ext (Range (Z :. limit) (Z :. sh   ) outerRange) outer $
       AUndefined ext


{-# INLINE csr #-}
csr :: (Source r Double)
    => Array r DIM1 Double 
    -> Array D DIM1 Double
csr !xs = unsafeBackpermute ext shift xs
    where
      {-# INLINE shift #-}
      shift !(Z :. 0) = Z :. (sh - 1)
      shift !(Z :. i) = Z :. ( i - 1)
      !ext = extent xs
      !sh  = size ext


{-# INLINE csrP #-}
csrP :: (Source r Double) 
     => Array r DIM1 Double 
     -> Array PS DIM1 Double
csrP !xs = 
    let !ext   = extent xs
        !sh    = size ext
        !limit = if sh == 0 then 0 else 1
        {-# INLINE innerRange #-}
        innerRange !(Z :. i) = i /= 0
        {-# INLINE outerRange #-}
        outerRange = not . innerRange
        inner =          unsafeBackpermute ext (\(Z :. i) -> Z :. (i  - 1)) xs
        outer = ASmall $ unsafeBackpermute ext (\_        -> Z :. (sh - 1)) xs
    in APart ext (Range (Z :. limit) (Z :. sh   ) innerRange) inner $ 
       APart ext (Range (Z :. 0    ) (Z :. limit) outerRange) outer $
       AUndefined ext


{-# INLINE cslN #-}
cslN :: (Source r Double)
     => Int
     -> Array r DIM1 Double 
     -> Array D DIM1 Double
cslN !m !xs = unsafeBackpermute ext shift xs
    where
      !n | sh == 0   = 0 -- See: Note [Preventing division by 0]
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
      !n | sh == 0    = 0 -- See: Note [Preventing division by 0]
         | otherwise  = m `mod` sh :: Int
      shift !(Z :. i) = if i >= n
                        then Z :. (i - n) 
                        else Z :. (i - n + sh)
      !ext = extent xs
      !sh  = size ext


instance Elt e => LoadRange D DIM1 e where
  {-# INLINE loadRangeP #-}
  loadRangeP (ADelayed _ getElem) mvec (Z :. start) (Z :. end)
    = mvec `deepSeqMVec` do  
      traceEventIO "Repa.loadRangeP[Delayed DIM1]: start"
      fillBlock1P (unsafeWriteMVec mvec) getElem start end
      touchMVec mvec
      traceEventIO "Repa.loadRangeP[Delayed DIM1]: end"


  {-# INLINE loadRangeS #-}
  loadRangeS (ADelayed _ getElem) mvec (Z :. start) (Z :. end)
    = mvec `deepSeqMVec` do  
      traceEventIO "Repa.loadRangeS[Delayed DIM1]: start"
      fillBlock1S (unsafeWriteMVec mvec) getElem start end
      touchMVec mvec
      traceEventIO "Repa.loadRangeS[Delayed DIM1]: end"


{-# INLINE fillBlock1S #-}
fillBlock1S :: (Int  -> a -> IO ())
            -> (DIM1 -> a)
            -> Int
            -> Int
            -> IO ()
fillBlock1S write getElem !start !end = fillBlock start
    where fillBlock !y
              | y >= end  = return ()
              | otherwise = do 
                  write y (getElem (Z :. y))
                  fillBlock (y + 1)
          {-# INLINE fillBlock #-}


{-# INLINE fillBlock1P #-}
fillBlock1P :: (Int  -> a -> IO ())
            -> (DIM1 -> a)
            -> Int
            -> Int
            -> IO ()
fillBlock1P write getElem !start !end = do
    gangIO theGang $ \(threadId) ->
              let !from = splitIx  threadId
                  !to   = splitIx (threadId + 1)
              in  fillBlock1S write getElem from to
    where
      !threads       = gangSize theGang
      !len           = end - start
      !chunkLen      = len `quot` threads
      !chunkLeftover = len `rem`  threads

      {-# INLINE splitIx #-}
      splitIx !thread
          | thread < chunkLeftover = start + thread * (chunkLen + 1)
          | otherwise              = start + thread * chunkLen  + chunkLeftover


{-

Note [Higher order functions interfere with fusion]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Creating two specialized workers that differ only with forceS/forceP function
is a lot of boilerplate. It would be better to implement dwtWorker as a higher
order function and pass either forceS or forceP as a parameter. Such approach 
however interferes with fusion for unknown reason. I noticed that when
I passed forceS/forceP as a parameter to dwt, but ignored it and passed concrete
force to dwtWorker like this:

dwt force angles signal = dwtWorker forceS csl angles signal

then fusion worked.


Note [Preventing division by 0]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Guards are needed because n is strict. If it were lazy, backpermute would not
perform computations for empty array and thus n wouldn't be calculated. However
when n is forced with a bang pattern it is always evaluated and hence it will
cause division by 0 error for `mod` operator.

-}
