{-# LANGUAGE BangPatterns #-}
module Signal.Wavelet.List.Common where 

import Control.Arrow ((&&&))


dwtWorker :: ((Double, Double) -> [Double] -> [Double])
          -> ([Double] -> [Double]) 
          -> [Double] 
          -> [Double] 
          -> [Double]
dwtWorker lattice layerTransition angles signal = go weights signal
    where
      go [w] sig    = lattice w sig
      go (w:ws) sig = go ws (layerTransition $ lattice w sig)
      go _ sig      = sig
      weights       = a2w angles


latticeSeq :: (Double, Double) -> [Double] -> [Double]
latticeSeq _ []  = []
latticeSeq _ [_] = []
latticeSeq !(!s, !c) (x1:x2:xs) = x1 `seq` x2 `seq` 
                                  x1 * c + x2 * s : 
                                  x1 * s - x2 * c : 
                                  latticeSeq (s, c) xs


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


csl :: [Double] -> [Double]
csl [] = []
csl (x:xs) = xs ++ [x]


csr :: [Double] -> [Double]
csr [] = []
csr xs = last xs : init xs


cslN :: Int -> [Double] -> [Double]
cslN !n sig
    | n > 0     = cslN (n - 1) (csl sig)
    | n == 0    = sig
    | otherwise = csrN (-n) sig


csrN :: Int -> [Double] -> [Double]
csrN !n sig
    | n > 0     = csrN (n - 1) (csr sig)
    | n == 0    = sig
    | otherwise = cslN (-n) sig


a2w :: [Double] -> [(Double,Double)]
a2w = map (sin &&& cos)


inv :: [Double] -> [Double]
inv = reverse


toDeg :: [Double] -> [Double]
toDeg = map (\x -> x * 180 / pi)


toRad :: [Double] -> [Double]
toRad = map (\x -> x * pi / 180)


chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = as : chunk n bs where !(as, bs) = splitAt n xs
