{-# LANGUAGE FlexibleContexts #-}

module Signal.Wavelet.Vector.Common where

import Data.Vector.Generic as V

inv :: Vector v Double => v Double -> v Double
inv = V.reverse


toDeg :: Vector v Double => v Double -> v Double
toDeg = V.map (\x -> x * 180 / pi)


toRad :: Vector v Double => v Double -> v Double
toRad = V.map (\x -> x * pi / 180)
