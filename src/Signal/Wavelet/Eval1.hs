{-# LANGUAGE BangPatterns #-}

module Signal.Wavelet.Eval1 where

import Control.Parallel.Strategies
import Signal.Wavelet.List.Common as LC

dwt :: [Double] -> [Double] -> [Double]
dwt angles signal =  dwtWorker csl angles signal


idwt :: [Double] -> [Double] -> [Double]
idwt angles signal = dwtWorker csr angles signal


dwtWorker :: ([Double] -> [Double]) -> [Double] -> [Double] -> [Double]
dwtWorker cs angles signal = go weights signal
    where
      go [w] sig    = Signal.Wavelet.Eval1.lattice w sig
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
      latticePar ys = return . LC.lattice (s, c) $ ys


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
        x'   <- rpar $ LC.lattice (s, c) x
        xss' <- go xss
        return (x' ++ xss')

