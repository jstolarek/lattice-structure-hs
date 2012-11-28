{-# LANGUAGE 
  BangPatterns,
  EmptyDataDecls,
  FlexibleInstances, 
  MagicHash,
  MultiParamTypeClasses, 
  TypeFamilies
  #-}
module Signal.Wavelet.Repa3 where

import Data.Array.Repa hiding (map)
import Data.Array.Repa.Eval.Gang
import Data.Array.Repa.Eval.Load
import Data.Array.Repa.Eval.Target
import Debug.Trace

import Signal.Wavelet.Repa.Common (forceS, forceP)

data L

instance Source L Double where

    data Array L sh Double
        = ALattice 
          { lLength :: sh -- only DIM1
          , lBaseOp :: !(Double, Double)
          , lGetSig :: Int -> Double
          }

    extent = lLength

    deepSeqArray (ALattice sh (s, c) getSig) y = 
        sh `deepSeq` s `seq` c `seq` getSig `seq` y

    linearIndex (ALattice _ (!s, !c) f) i
             | even i    = let x = f i
                               y = f (i + 1)
                           in x * c + y * s
             | otherwise = let x = f (i - 1)
                               y = f i
                           in x * s - y * c


instance Load L DIM1 Double where
  {-# INLINE [4] loadP #-}
  loadP (ALattice (Z :. l) (s, c) getSig) mvec
    = mvec `deepSeqMVec` do
      traceEventIO "Repa.loadP[Lattice]: start"
      fillLatticeP (unsafeWriteMVec mvec) getSig
                   s c l
      touchMVec mvec
      traceEventIO "Repa.loadP[Lattice]: end"
 

  {-# INLINE [4] loadS #-}
  loadS (ALattice (Z :. l) (s, c) getSig) mvec
    = mvec `deepSeqMVec` do
      traceEventIO "Repa.loadS[Lattice]: start"
      fillLatticeS (unsafeWriteMVec mvec) getSig
                   s c 0 l
      touchMVec mvec
      traceEventIO "Repa.loadS[Lattice]: end"


{-# INLINE latticeS #-}
latticeS :: (Double, Double) -> Array U DIM1 Double -> Array U DIM1 Double
latticeS (s, c) sig = forceS $ lattice (s, c) sig


{-# INLINE latticeP #-}
latticeP :: (Double, Double) -> Array U DIM1 Double -> Array U DIM1 Double
latticeP (s, c) sig = forceP $ lattice (s, c) sig


{-# INLINE lattice #-}
lattice :: (Double, Double) -> Array U DIM1 Double -> Array L DIM1 Double
lattice (s, c) sig = ALattice (extent sig) (s, c) (linearIndex sig)


fillLatticeP :: (Int -> Double -> IO ())
             -> (Int -> Double)
             -> Double
             -> Double
             -> Int
             -> IO ()
fillLatticeP write getElem s c sigLength = 
    -- this algorithm adapted from Data.Array.Repa.Eval.Chunked.hs
    -- from Repa library
    gangIO theGang $ \(threadId) ->
              let !start   = splitIx  threadId
                  !end     = splitIx (threadId + 1)
              in  fillLatticeS write getElem s c start end
    where
      !threads       = gangSize theGang
      !baseOps       = sigLength `quot` 2
      !chunkLen      = 2 * (baseOps `quot` threads)
      !chunkLeftover =      baseOps `rem`  threads

      {-# INLINE splitIx #-}
      splitIx thread
          | thread < chunkLeftover = thread * (chunkLen + 2)
          | otherwise              = thread * chunkLen  + 2 * chunkLeftover


fillLatticeS :: (Int -> Double -> IO ())
             -> (Int -> Double)
             -> Double
             -> Double
             -> Int
             -> Int
             -> IO ()
fillLatticeS write getElem !s !c !start !end = fillLattice start
    where fillLattice !offset
              | offset == end = return ()
              | otherwise     = do
                  let !x = getElem offset
                      !y = getElem (offset + 1)
                  write  offset      (x * c + y * s)
                  write (offset + 1) (x * s - y * c)
                  fillLattice (offset + 2)
