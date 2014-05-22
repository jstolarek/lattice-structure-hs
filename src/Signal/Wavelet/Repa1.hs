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
lattice :: (Source r Double, Shape sh)
        => (Double, Double)
        -> Array r (sh :. Int) Double
        -> Array D (sh :. Int) Double
lattice !(!s, !c) !xs = fromPairs . R.map baseOp . toPairs $ xs
    where
      baseOp !(!x1, !x2) = (x1 * c + x2 * s,  x1 * s - x2 * c)


{-# INLINE toPairs #-}
toPairs :: (Source r Double, Shape sh)
        => Array r (sh :. Int) Double
        -> Array D (sh :. Int) (Double, Double)
toPairs !xs = unsafeTraverse xs twiceShorter wrapPairs
    where
      {-# INLINE twiceShorter #-}
      twiceShorter !(sh :. s) = sh :. s `quot` 2
      {-# INLINE wrapPairs #-}
      wrapPairs f  !(sh :. i) = (f (sh :. 2 * i ), f (sh :. 2 * i + 1))


{-# INLINE fromPairs #-}
fromPairs :: (Source r (Double,Double), Shape sh)
          => Array r (sh :. Int) (Double, Double)
          -> Array D (sh :. Int) Double
fromPairs !xs = unsafeTraverse xs twiceLonger unwrapPairs
    where
      {-# INLINE twiceLonger #-}
      twiceLonger !(sh :. s) = sh :. 2 * s
      {-# INLINE unwrapPairs #-}
      unwrapPairs f !(sh :. i)
                      | even i    = fst . f $ (sh :. i `quot` 2)
                      | otherwise = snd . f $ (sh :. i `quot` 2)


{-# INLINE a2w #-}
a2w :: (Source r Double)
    => Array r DIM1 Double
    -> Array D DIM1 (Double, Double)
a2w = R.map (sin &&& cos)


{-# INLINE csl #-}
csl :: (Source r Double, Shape sh)
    => Array r (sh :. Int) Double
    -> Array D (sh :. Int) Double
csl !xs = unsafeBackpermute ext shift xs
    where
      {-# INLINE shift #-}
      shift !(sh :. i) = if i /= (dim - 1)
                         then sh :. (i + 1)
                         else sh :. 0
      !ext = extent xs
      !dim = size ext


{-# INLINE cslP #-}
cslP :: (Source r Double, Shape sh)
     => Array r  (sh :. Int) Double
     -> Array PS (sh :. Int) Double
cslP !xs =
    let !ext@(ex :. _) = extent xs
        !dim           = size ext
        !limit         = max 0 (dim - 1)
        {-# INLINE innerRange #-}
        innerRange !(_ :. i) = i /= (dim - 1)
        {-# INLINE outerRange #-}
        outerRange = not . innerRange
        inner =          unsafeBackpermute ext (\(sh :. i) -> sh :. (i + 1)) xs
        outer = ASmall $ unsafeBackpermute ext (\(sh :. _) -> sh :. 0      ) xs
    in APart ext (Range (ex :. 0)     (ex :. limit) innerRange) inner $
       APart ext (Range (ex :. limit) (ex :. dim  ) outerRange) outer $
       AUndefined ext


{-# INLINE csr #-}
csr :: (Source r Double, Shape sh)
    => Array r (sh :. Int) Double
    -> Array D (sh :. Int) Double
csr !xs = unsafeBackpermute ext shift xs
    where
      {-# INLINE shift #-}
      shift !(sh :. 0) = sh :. (dim - 1)
      shift !(sh :. i) = sh :. (  i - 1)
      !ext = extent xs
      !dim = size ext


{-# INLINE csrP #-}
csrP :: (Source r Double, Shape sh)
     => Array r  (sh :. Int) Double
     -> Array PS (sh :. Int) Double
csrP !xs =
    let !ext@(ex :. _) = extent xs
        !len           = size ext
        !limit         = if len == 0 then 0 else 1
        {-# INLINE innerRange #-}
        innerRange !(_ :. i) = i /= 0
        {-# INLINE outerRange #-}
        outerRange = not . innerRange
        inner =          unsafeBackpermute ext (\(sh:. i) -> sh :. (i   - 1)) xs
        outer = ASmall $ unsafeBackpermute ext (\(sh:. _) -> sh :. (len - 1)) xs
    in APart ext (Range (ex :. limit) (ex :. len  ) innerRange) inner $
       APart ext (Range (ex :. 0    ) (ex :. limit) outerRange) outer $
       AUndefined ext


{-# INLINE cslN #-}
cslN :: (Source r Double, Shape sh)
     => Int
     -> Array r (sh :. Int) Double
     -> Array D (sh :. Int) Double
cslN !m !xs = unsafeBackpermute ext shift xs
    where
      !n | len == 0  = 0 -- See: Note [Preventing division by 0]
         | otherwise = m `mod` len :: Int
      {-# INLINE shift #-}
      shift (sh :. i) = if i < (len - n)
                        then sh :. (i + n)
                        else sh :. (i + n - len)
      !ext = extent xs
      !len = size ext


{-# INLINE csrN #-}
csrN :: (Source r Double, Shape sh)
     => Int
     -> Array r (sh :. Int) Double
     -> Array D (sh :. Int) Double
csrN !m xs = unsafeBackpermute ext shift xs
    where
      !n | len == 0   = 0 -- See: Note [Preventing division by 0]
         | otherwise  = m `mod` len :: Int
      {-# INLINE shift #-}
      shift !(sh :. i) = if i >= n
                         then sh :. (i - n)
                         else sh :. (i - n + len)
      !ext = extent xs
      !len = size ext


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


-- Note [Higher order functions interfere with fusion]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- Creating two specialized workers that differ only with forceS/forceP function
-- is a lot of boilerplate. It would be better to implement dwtWorker as a
-- higher order function and pass either forceS or forceP as a parameter. Such
-- approach however interferes with fusion for unknown reason. I noticed that
-- when I passed forceS/forceP as a parameter to dwt, but ignored it and passed
-- concrete force to dwtWorker like this:
--
--   dwt force angles signal = dwtWorker forceS csl angles signal
--
-- then fusion worked.


-- Note [Preventing division by 0]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- Guards are needed because n is strict. If it were lazy, backpermute would not
-- perform computations for empty array and thus n wouldn't be
-- calculated. However when n is forced with a bang pattern it is always
-- evaluated and hence it will cause division by 0 error for `mod` operator.
