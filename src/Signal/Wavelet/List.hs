module Signal.Wavelet.List where

import Control.Arrow

type LS = [Double]


dwt :: LS -> [Double] -> [Double]
dwt angles signal =  dwtWorker csl angles signal


idwt :: LS -> [Double] -> [Double]
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


a2w :: [Double] -> [(Double,Double)]
a2w = map (sin &&& cos)


inv :: LS -> LS
inv = reverse


csl :: [Double] -> [Double]
csl [] = []
csl (x:xs) = xs ++ [x]


csr :: [Double] -> [Double]
csr [] = []
csr xs = last xs : init xs


toDeg :: [Double] -> [Double]
toDeg = map (\x -> x * 180 / pi)


toRad :: [Double] -> [Double]
toRad = map (\x -> x * pi / 180)