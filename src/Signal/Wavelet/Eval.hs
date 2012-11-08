module Signal.Wavelet.Eval where

import Control.DeepSeq
import Control.Monad
import Control.Parallel.Strategies
import Signal.Wavelet.List.Common

dwt :: [Double] -> [Double] -> [Double]
dwt angles signal =  dwtWorker csl angles signal


idwt :: [Double] -> [Double] -> [Double]
idwt angles signal = dwtWorker csr angles signal


dwtWorker :: ([Double] -> [Double]) -> [Double] -> [Double] -> [Double]
dwtWorker cs angles signal = go weights signal
    where
      go [w] sig    = lattice w sig
      go (w:ws) sig = go ws (cs . lattice w $ sig)
      go _ sig      = sig
      weights       = a2w angles


lattice :: (Double, Double) -> [Double] -> [Double]
--lattice _ []  = []
--lattice _ [_] = []
lattice (s, c) xss = force . runEval . go $ xss
    where
      go []  = return []
      go [_] = return []
      go (x:y:xs) = do 
        (x',y') <- liftM force (rpar baseOp)
        xs'     <- go xs
        return (x':y':xs')
          where baseOp = (x * c + y * s,  x * s - y * c)
      

--FIXME usunąć
csl :: [Double] -> [Double]
csl [] = []
csl (x:xs) = xs ++ [x]


csr :: [Double] -> [Double]
csr [] = []
csr xs = last xs : init xs
