module Signal.Wavelet.Eval1 where

import Signal.Wavelet.Eval.Common
import Signal.Wavelet.List.Common


dwt :: [Double] -> [Double] -> [Double]
dwt angles signal =  dwtWorker latticePar csl angles signal


idwt :: [Double] -> [Double] -> [Double]
idwt angles signal = dwtWorker latticePar csr angles signal
