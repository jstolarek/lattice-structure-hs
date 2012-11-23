module Signal.Wavelet.Eval1 where

import Signal.Wavelet.Eval.Common (latticePar)
import Signal.Wavelet.List.Common (dwtWorker, csl, csr)


dwt :: [Double] -> [Double] -> [Double]
dwt angles signal =  dwtWorker latticePar csl angles signal


idwt :: [Double] -> [Double] -> [Double]
idwt angles signal = dwtWorker latticePar csr angles signal
