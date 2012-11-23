{-# LANGUAGE FlexibleContexts #-}
module Signal.Wavelet.Vector.Common where

import Data.Vector.Generic as V (Vector, reverse, map)


{-# INLINE inv #-}
inv :: Vector v Double => v Double -> v Double
inv = V.reverse


{-# INLINE toDeg #-}
toDeg :: Vector v Double => v Double -> v Double
toDeg = V.map (\x -> x * 180 / pi)


{-# INLINE toRad #-}
toRad :: Vector v Double => v Double -> v Double
toRad = V.map (\x -> x * pi / 180)
