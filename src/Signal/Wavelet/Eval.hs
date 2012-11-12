{-# LANGUAGE BangPatterns #-}

module Signal.Wavelet.Eval where

import Control.DeepSeq
import Control.Parallel.Strategies
--import Data.DList
import Signal.Wavelet.List.Common

dwt :: [Double] -> [Double] -> [Double]
dwt angles signal =  dwtWorker csl angles signal


idwt :: [Double] -> [Double] -> [Double]
idwt angles signal = dwtWorker csr angles signal


dwtWorker :: ([Double] -> [Double]) -> [Double] -> [Double] -> [Double]
dwtWorker cs angles signal = go weights signal
    where
      go [w] sig    = lattice w sig
      go (w:ws) sig = go ws (cs . lattice3 w $ sig)
      go _ sig      = sig
      weights       = a2w angles


lattice :: (Double, Double) -> [Double] -> [Double]
lattice (s, c) xss = runEval . go $ xss
    where
      go []  = return []
      go [_] = return []
      go (x:y:xs) = do 
        x'  <- rpar $ x * c + y * s
        y'  <- rpar $ x * s - y * c
        xs' <- go xs
        return (x':y':xs')


lattice3 :: (Double, Double) -> [Double] -> [Double]
lattice3 !(!s, !c) xs = runEval $ do
  xss <- parList (latticePar `dot` rdeepseq) (chunk 8192 xs)
  return . concat $ xss
    where
      latticePar :: Strategy [Double]
      latticePar ys = return . latticeSeq (s, c) $ ys


chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = as : chunk n bs where (as,bs) = splitAt n xs


lattice2 :: (Double, Double) -> [Double] -> [Double]
lattice2 !(!s, !c) ys = runEval . go $ ys
    where
      go :: Strategy [Double]
      go [] = return []
      go xs = do 
        let (x, xss) = splitAt 256 xs
        x'   <- rpar $ latticeSeq (s, c) x
        xss' <- go xss
        return (x' ++ xss')


latticeSeq :: (Double, Double) -> [Double] -> [Double]
latticeSeq _ [] = []
latticeSeq !(!s, !c) (x1:x2:xs) = x1 * c + x2 * s : 
                                  x1 * s - x2 * c : 
                                  lattice (s,c) xs
latticeSeq _ _ = error "Can't perform a wavelet transform of odd length signal"
