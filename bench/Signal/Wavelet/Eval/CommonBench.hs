module Signal.Wavelet.Eval.CommonBench where

import Control.Arrow ((&&&))
import Signal.Wavelet.Eval.Common


{-# INLINE benchLattice #-}
benchLattice :: ((Double, Double), [Double]) -> [Double]
benchLattice (baseOp, sig) = latticePar baseOp sig


dataLattice :: ([Double], [Double])
            -> ((Double, Double), [Double])
dataLattice (ls, sig) = ((sin &&& cos) . head $ ls , sig)
