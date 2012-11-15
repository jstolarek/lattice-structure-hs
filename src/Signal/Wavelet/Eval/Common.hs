{-# LANGUAGE BangPatterns #-}
module Signal.Wavelet.Eval.Common where

import Control.Applicative
import Control.Parallel.Strategies
import Signal.Wavelet.List.Common as LC


latticePar :: (Double, Double) -> [Double] -> [Double]
latticePar (s, c) xss = runEval $ 
    concat <$> parList (rdeepseq . LC.latticeSeq (s, c)) (chunk 2048 xss)
                   

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = as : chunk n bs where (as,bs) = splitAt n xs
