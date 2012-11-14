module Signal.Wavelet.List1 where

import Signal.Wavelet.List.Common


dwt :: [Double] -> [Double] -> [Double]
dwt angles signal  = dwtWorker latticeSeq csl angles signal


idwt :: [Double] -> [Double] -> [Double]
idwt angles signal = dwtWorker latticeSeq csr angles signal
