{-# LANGUAGE BangPatterns #-}

module Signal.Wavelet.List.Common where 

import Control.Arrow ((&&&))

lattice :: (Double, Double) -> [Double] -> [Double]
lattice _ []  = []
lattice _ [_] = []
lattice !(!s, !c) (x1:x2:xs) = x1 `seq` x2 `seq` 
                               x1 * c + x2 * s : 
                               x1 * s - x2 * c : 
                               lattice (s, c) xs


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


a2w :: [Double] -> [(Double,Double)]
a2w = map (sin &&& cos)


inv :: [Double] -> [Double]
inv = reverse


toDeg :: [Double] -> [Double]
toDeg = map (\x -> x * 180 / pi)


toRad :: [Double] -> [Double]
toRad = map (\x -> x * pi / 180)
