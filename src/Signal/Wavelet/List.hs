module Signal.Wavelet.List where

import Signal.Wavelet.List.Common

dwt :: [Double] -> [Double] -> [Double]
dwt angles signal =  dwtWorker csl angles signal


idwt :: [Double] -> [Double] -> [Double]
idwt angles signal = dwtWorker csr angles signal


dwtWorker :: ([Double] -> [Double]) -> [Double] -> [Double] -> [Double]
dwtWorker cs angles signal = go weights signal
    where
      go [w] sig    = lattice w sig
      go (w:ws) sig = go ws (cs $ lattice w sig)
      go _ sig      = sig
      weights       = a2w angles

lattice :: (Double, Double) -> [Double] -> [Double]
lattice _ [] = []
lattice (s, c) (x1:x2:xs) = x1 * c + x2 * s : 
                            x1 * s - x2 * c : 
                            lattice (s,c) xs
lattice _ _ = error "Can't perform a wavelet transform of odd length signal"


csl :: [Double] -> [Double]
csl [] = []
csl (x:xs) = xs ++ [x]


csr :: [Double] -> [Double]
csr [] = []
csr xs = last xs : init xs


cslN :: Int -> [Double] -> [Double]
cslN n sig
    | n > 0     = cslN (n-1) (csl sig)
    | n == 0    = sig
    | otherwise = csrN (-n) sig


csrN :: Int -> [Double] -> [Double]
csrN n sig
    | n > 0     = csrN (n-1) (csr sig)
    | n == 0    = sig
    | otherwise = cslN (-n) sig
