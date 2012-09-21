module Signal.Wavelet where

dwt :: [Double] -> [Double]
dwt [] = []
dwt _ = undefined

latticeLayer :: (Double, Double) -> [Double] -> [Double]
latticeLayer _ [] = []
latticeLayer (s, c) (x1:x2:xs) = x1 * c + x2 * s : 
                                 x1 * s - x2 * c : 
                                 latticeLayer (s,c) xs
latticeLayer _ _ = error "Odd length"

cyclicShiftLeft :: [Double] -> [Double]
cyclicShiftLeft [] = []
cyclicShiftLeft xs = tail xs ++ [head xs]

cyclicShiftRight :: [Double] -> [Double]
cyclicShiftRight [] = []
cyclicShiftRight xs = last xs : init xs

idwt :: [Double] -> [Double]
idwt _ = undefined
