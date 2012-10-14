module Signal.Wavelet where

type LS = [Double]

dwt :: LS -> [Double] -> [Double]
dwt angles signal = cyclicShiftRight $ foldl doLayer signal weights
    where
      doLayer sig wei = cyclicShiftLeft (latticeLayer wei sig)
      weights = anglesToWeights angles

idwt :: LS -> [Double] -> [Double]
idwt angles signal = cyclicShiftLeft $ foldl doLayer signal weights
    where
      doLayer sig wei = cyclicShiftRight (latticeLayer wei sig)
      weights = anglesToWeights angles

latticeLayer :: (Double, Double) -> [Double] -> [Double]
latticeLayer _ [] = []
latticeLayer (s, c) (x1:x2:xs) = x1 * c + x2 * s : 
                                 x1 * s - x2 * c : 
                                 latticeLayer (s,c) xs
--FIXME
latticeLayer _ _ = error "Odd length"

anglesToWeights :: [Double] -> [(Double,Double)]
anglesToWeights = map (\x -> (sin x, cos x))

invLS :: LS -> LS
invLS = reverse

cyclicShiftLeft :: [Double] -> [Double]
cyclicShiftLeft [] = []
cyclicShiftLeft (x:xs) = xs ++ [x]

cyclicShiftRight :: [Double] -> [Double]
cyclicShiftRight [] = []
cyclicShiftRight xs = last xs : init xs

toDeg :: [Double] -> [Double]
toDeg = map (\x -> x * 180 / pi)

toRad :: [Double] -> [Double]
toRad = map (\x -> x * pi / 180)
