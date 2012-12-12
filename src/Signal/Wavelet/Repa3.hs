{-# LANGUAGE 
  BangPatterns,
  EmptyDataDecls,
  FlexibleContexts,
  FlexibleInstances,
  MagicHash,
  MultiParamTypeClasses, 
  TypeFamilies
  #-}
module Signal.Wavelet.Repa3 where

import Control.Arrow               ((&&&))
import Data.Array.Repa             hiding (map)
import Data.Array.Repa.Eval.Gang   (theGang, gangIO, gangSize)
import Data.Array.Repa.Eval.Load   (Load(..))
import Data.Array.Repa.Eval.Target (Target(..))
import Debug.Trace                 (traceEventIO)

import Signal.Wavelet.Repa.Common  (forceS, forceP)

data L

instance Source L Double where

    data Array L sh Double
        = ALattice 
          { lLength   :: sh -- only DIM1
          , lBaseOp   :: !(Double, Double)
          , lGetSig   :: Int -> Double
          , lModifier :: Int
          }

    extent = lLength

    deepSeqArray (ALattice sh (s, c) getSig lm) y = 
        sh `deepSeq` s `seq` c `seq` getSig `seq` lm `seq` y

    linearIndex (ALattice _ (!s, !c) f _) i
             | even i    = let x = f i
                               y = f (i + 1)
                           in x * c + y * s
             | otherwise = let x = f (i - 1)
                               y = f i
                           in x * s - y * c


instance Load L DIM1 Double where
  {-# INLINE loadP #-}
  loadP (ALattice (Z :. l) (s, c) getSig lm) mvec
    = mvec `deepSeqMVec` do
      traceEventIO "Repa.loadP[Lattice]: start"
      fillLatticeP (unsafeWriteMVec mvec) getSig lm s c l
      touchMVec mvec
      traceEventIO "Repa.loadP[Lattice]: end"
 

  {-# INLINE loadS #-}
  loadS (ALattice (Z :. l) (s, c) getSig lm) mvec
    = mvec `deepSeqMVec` do
      traceEventIO "Repa.loadS[Lattice]: start"
      fillLatticeS (unsafeWriteMVec mvec) getSig lm s c 0 l (l - 1)
      touchMVec mvec
      traceEventIO "Repa.loadS[Lattice]: end"

dwtS, dwtP :: Array U DIM1 Double
           -> Array U DIM1 Double 
           -> Array U DIM1 Double
dwtS angles signal = dwtWorkerS 0 layersCount layerModifier angles signal
    where
      layerModifier = 0
      layersCount   = size . extent $ angles


dwtP angles signal = dwtWorkerP 0 layersCount layerModifier angles signal
    where
      layerModifier = 0
      layersCount   = size . extent $ angles


idwtS, idwtP :: Array U DIM1 Double
             -> Array U DIM1 Double 
             -> Array U DIM1 Double
idwtS angles signal = dwtWorkerS 0 layersCount layerModifier angles signal
    where
      layerModifier | even layersCount = 1
                    | otherwise        = 0
      layersCount   = size . extent $ angles


idwtP angles signal = dwtWorkerP 0 layersCount layerModifier angles signal
    where
      layerModifier | even layersCount = 1
                    | otherwise        = 0
      layersCount   = size . extent $ angles


{-# INLINE dwtWorkerS #-}
dwtWorkerS, dwtWorkerP :: Int 
                       -> Int
                       -> Int
                       -> Array U DIM1 Double 
                       -> Array U DIM1 Double 
                       -> Array U DIM1 Double
dwtWorkerS !currentLayer !layersCount !layerModifier angles signal
    | currentLayer == layersCount = signal
    | otherwise = dwtWorkerS nextLayer layersCount newModifier angles newSignal
    where
      nextLayer    = currentLayer + 1
      newModifier  = 1 - layerModifier
      (sin_, cos_) = (sin &&& cos) $ angles `unsafeIndex` (Z :. currentLayer)
      newSignal    = forceS $ lattice layerModifier (sin_, cos_) signal


{-# INLINE dwtWorkerP #-}
dwtWorkerP !currentLayer !layersCount !layerModifier angles signal
    | currentLayer == layersCount = signal
    | otherwise = dwtWorkerP nextLayer layersCount newModifier angles newSignal
    where
      nextLayer    = currentLayer + 1
      newModifier  = 1 - layerModifier
      (sin_, cos_) = (sin &&& cos) $ angles `unsafeIndex` (Z :. currentLayer)
      newSignal    = forceP $ lattice layerModifier (sin_, cos_) signal


{-# INLINE lattice #-}
lattice :: Int 
        -> (Double, Double) 
        -> Array U DIM1 Double 
        -> Array L DIM1 Double
lattice lm (s, c) sig = ALattice (extent sig) (s, c) (unsafeLinearIndex sig) lm


{-# INLINE fillLatticeP #-}
fillLatticeP :: (Int -> Double -> IO ())
             -> (Int -> Double)
             -> Int
             -> Double
             -> Double
             -> Int
             -> IO ()
fillLatticeP write getElem !lm !s !c !sigLength = 
    -- this algorithm adapted from Data.Array.Repa.Eval.Chunked.hs
    -- from Repa library
    gangIO theGang $ \(threadId) ->
              let !start   = splitIx  threadId
                  !end     = splitIx (threadId + 1)
              in  fillLatticeS write getElem lm s c start end (sigLength - 1)
    where
      !threads       = gangSize theGang
      !baseOps       = sigLength `quot` 2
      !chunkLen      = 2 * (baseOps `quot` threads)
      !chunkLeftover =      baseOps `rem`  threads

      {-# INLINE splitIx #-}
      splitIx thread
          | thread < chunkLeftover = thread * (chunkLen + 2)
          | otherwise              = thread * chunkLen  + 2 * chunkLeftover


{-# INLINE fillLatticeS #-}
fillLatticeS :: (Int -> Double -> IO ())
             -> (Int -> Double)
             -> Int
             -> Double
             -> Double
             -> Int
             -> Int
             -> Int
             -> IO ()
fillLatticeS write getElem !lm !s !c !start !end !lastE = 
    fillLattice (start + lm)
    where fillLattice !offset
              | offset >= end   = return ()
              | offset == lastE = do
                  let !x = getElem lastE
                      !y = getElem 0
                  write lastE (x * c + y * s)
                  write 0     (x * s - y * c)
                  return ()
              | otherwise = do
                  let !x = getElem offset
                      !y = getElem (offset + 1)
                  write  offset      (x * c + y * s)
                  write (offset + 1) (x * s - y * c)
                  fillLattice (offset + 2)
