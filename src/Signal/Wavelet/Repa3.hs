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
import Data.Vector.Unboxed as U (Vector, fromList, (!))
import Debug.Trace

import Signal.Wavelet.Repa.Common

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
lattice (s, c) sig = forceP lat
    where lat = ALattice (extent sig) (s, c) (linearIndex sig)


fillLatticeP :: (Int -> Double -> IO ())
             -> (Int -> Double)
             -> Double
             -> Double
             -> Int
             -> IO ()
fillLatticeP write getElem s c count = gangIO theGang fillLattice
    where
      !threads    = gangSize theGang
      !chunkLen   = (count `quot` 2) `quot` threads
      !chunkSlack = (count `quot` 2) `rem`  threads
      !workShare  = distributeWork threads chunkLen chunkSlack

      fillLattice :: Int -> IO ()
      fillLattice threadId = 
          fillLatticeS write getElem s c 
                      (workShare U.! (threadId + 1))
                      (workShare U.! threadId)


fillLatticeS :: (Int -> Double -> IO ())
             -> (Int -> Double)
             -> Double
             -> Double
             -> Int
             -> Int
             -> IO ()
fillLatticeS write getElem !s !c count off = fillLattice off
    where fillLattice offset
              | offset == count = return ()
              | otherwise       = do
                  let !x = getElem offset
                      !y = getElem (offset + 1)
                  write  offset      (x * c + y * s)
                  write (offset + 1) (x * s - y * c)
                  fillLattice (offset + 2)


distributeWork :: Int -> Int -> Int -> Vector Int
distributeWork numCaps chunkLen chunkSlack = U.fromList . scanl (+) 0 . 
                       map (*2) . distribute numCaps chunkLen $ chunkSlack
    where 
      distribute !0    _    _   = []
      distribute !caps !len !0  = len     : distribute (caps - 1) len 0
      distribute !caps !len !sl = len + 1 : distribute (caps - 1) len (sl - 1)  
