module Signal.Wavelet.List2 where

import Signal.Wavelet.List.Common


dwt :: [Double] -> [Double] -> [Double]
dwt _ [] = []
dwt angles signal = dwtWorker latticeSeq tail angles extendedSignal
    where
      extendedSignal = extendEnd layers signal
      layers         = length angles


idwt :: [Double] -> [Double] -> [Double]
idwt _ [] = []
idwt angles signal = dwtWorker latticeSeq tail angles extendedSignal
    where
      extendedSignal = extendFront layers signal
      layers         = length angles
