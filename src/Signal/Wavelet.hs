module Signal.Wavelet where

dwt :: [Double] -> [Double]
dwt [] = []
dwt xs = undefined

latticeLayer :: (Double, Double) -> [Double] -> [Double]
latticeLayer _ [] = []
latticeLayer (s, c) (x1:x2:xs) = x1 * c + x2 * s : x1 * s - x2 * c : latticeLayer (s,c) xs
latticeLayer _ _ = error "Odd length"

shiftLeft :: [Double] -> [Double]
shiftLeft xs = tail xs ++ [head xs]

shiftRight :: [Double] -> [Double]
shiftRight xs = last xs : init xs

idwt :: [Double] -> [Double]
idwt xs = undefined
