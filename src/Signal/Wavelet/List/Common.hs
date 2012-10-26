module Signal.Wavelet.List.Common where 


toDeg :: [Double] -> [Double]
toDeg = map (\x -> x * 180 / pi)


toRad :: [Double] -> [Double]
toRad = map (\x -> x * pi / 180)
