module Signal.Wavelet.List.Common where 

import Control.Arrow ((&&&))

a2w :: [Double] -> [(Double,Double)]
a2w = map (sin &&& cos)


inv :: [Double] -> [Double]
inv = reverse


toDeg :: [Double] -> [Double]
toDeg = map (\x -> x * 180 / pi)


toRad :: [Double] -> [Double]
toRad = map (\x -> x * pi / 180)
