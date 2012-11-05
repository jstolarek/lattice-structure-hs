module Signal.Wavelet.Eval where

import Control.DeepSeq
import Control.Parallel.Strategies
import Signal.Wavelet.List.Common

dwt :: [Double] -> [Double] -> [Double]
dwt angles signal =  dwtWorker csl angles signal


idwt :: [Double] -> [Double] -> [Double]
idwt angles signal = dwtWorker csr angles signal


dwtWorker :: ([Double] -> [Double]) -> [Double] -> [Double] -> [Double]
dwtWorker cs angles signal = go weights signal
    where
      go [w] sig    = runEval $ lattice w sig
      go (w:ws) sig = go ws (cs . force . runEval . lattice w $ sig)
      go _ sig      = sig
      weights       = a2w angles


lattice :: (Double, Double) -> [Double] -> Eval [Double]
lattice _ []  = return []
lattice _ [_] = return []
lattice (s, c) (x:y:xs) = do
  (x',y') <- rpar baseOp
  xs'     <- lattice (s, c) xs
  return (x':y':xs')
      where baseOp = (x * c + y * s,  x * s - y * c)


--FIXME usunąć
csl :: [Double] -> [Double]
csl [] = []
csl (x:xs) = xs ++ [x]


csr :: [Double] -> [Double]
csr [] = []
csr xs = last xs : init xs
