{-# LANGUAGE BangPatterns #-}
module Signal.Wavelet.Eval.Common where

import Control.Parallel.Strategies
import Signal.Wavelet.List.Common as LC


latticePar :: (Double, Double) -> [Double] -> [Double]
latticePar (s, c) xss = runEval $ go xss
    where
      go :: Strategy [Double]
      go [] = return []
      go xs = do
          lsh <- (rpar `dot` rdeepseq) $ LC.latticeSeq (s, c) as
          lst <- go bs
          return (lsh ++ lst)
          where
            !(as, bs) = splitAt 512 xs
