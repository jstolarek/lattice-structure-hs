{-# LANGUAGE BangPatterns #-}

module Signal.Wavelet.List2 where

import Signal.Wavelet.List.Common

dwt :: [Double] -> [Double] -> [Double]
dwt _ [] = []
dwt angles signal = dwtWorker angles extendedSignal
    where 
      extendedSignal = extendEnd layers signal
      layers         = length angles


idwt :: [Double] -> [Double] -> [Double]
idwt _ [] = []
idwt angles signal = dwtWorker angles extendedSignal
    where
      extendedSignal = extendFront layers signal
      layers         = length angles


dwtWorker :: [Double] -> [Double] -> [Double]
dwtWorker angles signal = go weights signal
    where
      go [w] sig    = lattice w sig
      go (w:ws) sig = go ws (tail $ lattice w sig)
      go _ sig      = sig
      weights       = a2w angles


extendFront :: Int -> [Double] -> [Double]
extendFront = extendWorker (\sig sigSize extSize -> 
                                drop (sigSize - extSize) sig ++ sig)


extendEnd :: Int -> [Double] -> [Double]
extendEnd = extendWorker (\sig _ extSize -> sig ++ take extSize sig)


extendWorker :: ([Double] -> Int -> Int -> [Double]) 
             -> Int 
             -> [Double] 
             -> [Double]
extendWorker extBuilder !layers signal = go signal initExt initSigSize
    where !initExt     = 2 * layers - 2 :: Int
          !initSigSize = length signal  :: Int
          go sig !ln !sigSize
              | extSize <= 0   = sig
              | otherwise      = go extSignal (ln - extSize) (sigSize + extSize)
              where !extSize   = min sigSize ln :: Int
                    !extSignal = extBuilder sig sigSize extSize
