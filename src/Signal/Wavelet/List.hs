module Signal.Wavelet.List where

import Control.Arrow

type LS = [Double]


dwt :: LS -> [Double] -> [Double]
dwt angles signal = csr $ foldl doLayer signal weights
    where
      doLayer sig wei = csl $ lattice wei sig
      weights = a2w angles


idwt :: LS -> [Double] -> [Double]
idwt angles signal = csl $ foldl doLayer signal weights
    where
      doLayer sig wei = csr $ lattice wei sig
      weights = a2w angles


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
