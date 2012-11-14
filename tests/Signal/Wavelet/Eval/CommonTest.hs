module Signal.Wavelet.Eval.CommonTest where

import Signal.Wavelet.Eval.Common
import Signal.Wavelet.List.Common as LC
import Test.Utils

propLatticeIdenticalToList :: Double -> [Double] -> Bool
propLatticeIdenticalToList d xs = 
    LC.latticeSeq (s, c) ys =~ latticePar (s, c) ys
        where (s, c) = (sin d, cos d)
              ys     = xs ++ xs
