module Signal.Wavelet.Eval.Common where

import Control.Applicative         ((<$>))
import Control.Parallel.Strategies (parList, rdeepseq, runEval)
import Signal.Wavelet.List.Common  (latticeSeq, chunk)


latticePar :: (Double, Double) -> [Double] -> [Double]
latticePar (s, c) xss = runEval $ 
    concat <$> parList (rdeepseq . latticeSeq (s, c)) (chunk 2048 xss)
