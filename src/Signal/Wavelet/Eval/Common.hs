module Signal.Wavelet.Eval.Common where

import Control.Applicative
import Control.Parallel.Strategies
import Signal.Wavelet.List.Common as LC


latticePar :: (Double, Double) -> [Double] -> [Double]
latticePar (s, c) xss = runEval $ 
    concat <$> parList (rdeepseq . LC.latticeSeq (s, c)) (LC.chunk 2048 xss)
