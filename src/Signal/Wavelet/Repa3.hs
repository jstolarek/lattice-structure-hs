{-# LANGUAGE 
  BangPatterns,
  EmptyDataDecls,
  FlexibleInstances, 
  MagicHash,
  MultiParamTypeClasses, 
  TypeFamilies
  #-}
module Signal.Wavelet.Repa3 where

import Data.Array.Repa
import Data.Array.Repa.Eval.Gang
import Data.Array.Repa.Eval.Load
import Data.Array.Repa.Eval.Target
import Debug.Trace

import Signal.Wavelet.Repa.Common

data L

instance Source L Double where

    data Array L sh Double
        = ALattice 
          { layerLength :: sh -- only DIM1
          , baseOp      :: !(Double, Double)
          , getSig      :: Int -> Double
          }

    extent = layerLength

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
    {-# INLINE loadP #-}
    loadP (ALattice (Z :. l) (s, c) getSig) marr = do
      traceEventIO "Repa.loadP[Lattice]: start"
      fillLatticeP (unsafeWriteMVec marr) getSig
                   s c l
      touchMVec marr
      traceEventIO "Repa.loadP[Lattice]: end"
 

    {-# INLINE loadS #-}
    loadS (ALattice (Z :. l) (s, c) getSig) marr = do
      traceEventIO "Repa.loadS[Lattice]: start"
      fillLatticeS (unsafeWriteMVec marr) getSig
                   s c l 0
      touchMVec marr
      traceEventIO "Repa.loadS[Lattice]: end"


lattice :: (Double, Double) -> Array U DIM1 Double -> Array U DIM1 Double
lattice (s, c) sig = computeS lat
    where lat = ALattice (extent sig) (s, c) (linearIndex sig)


fillLatticeP :: (Int -> Double -> IO ())
             -> (Int -> Double)
             -> Double
             -> Double
             -> Int
             -> IO ()
fillLatticeP write getSig s c count = gangIO theGang fillLattice
    where 
      fillLattice :: Int -> IO ()
      fillLattice threadId = undefined


fillLatticeS :: (Int -> Double -> IO ())
             -> (Int -> Double)
             -> Double
             -> Double
             -> Int
             -> Int
             -> IO ()
fillLatticeS write getSig !s !c count = fillLattice
    where fillLattice offset
              | offset == count = return ()
              | otherwise       = do
                  let !x = getSig offset
                      !y = getSig (offset + 1)
                  write  offset      (x * c + y * s)
                  write (offset + 1) (x * s - y * c)
                  fillLattice (offset + 2)
