module Signal.Wavelet.List.CommonBench where

import Control.Arrow ((&&&))
import Signal.Wavelet.List.Common


{-# INLINE benchExtendFront #-}
benchExtendFront :: (Int, [Double]) -> [Double]
benchExtendFront (ls, sig) = extendFront ls sig


{-# INLINE benchExtendEnd #-}
benchExtendEnd :: (Int, [Double]) -> [Double]
benchExtendEnd (ls, sig) = extendEnd ls sig


{-# INLINE benchLattice #-}
benchLattice :: ((Double, Double), [Double]) -> [Double]
benchLattice (baseOp, sig) = latticeSeq baseOp sig


dataExtend :: ([Double], [Double])
           -> (Int, [Double])
dataExtend (ls, sig) = (length ls, sig)


dataLattice :: ([Double], [Double])
            -> ((Double, Double), [Double])
dataLattice (ls, sig) = ( (sin &&& cos) . head $ ls , sig)
