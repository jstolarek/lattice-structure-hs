module Signal.Wavelet.List.CommonTest where

import Signal.Wavelet.List.Common
import Test.Utils


propLatticeInverseInverse :: [Double] -> Bool
propLatticeInverseInverse xs = inv (inv xs) == xs


propDegRadInvertible :: [Double] -> Bool
propDegRadInvertible xs = toDeg (toRad xs) =~ xs


propRadDegInvertible :: [Double] -> Bool
propRadDegInvertible xs = toRad (toDeg xs) =~ xs
