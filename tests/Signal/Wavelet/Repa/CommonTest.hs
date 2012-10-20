module Signal.Wavelet.Repa.CommonTest where

import Data.Array.Repa
import Signal.Wavelet.Repa.Common
import Test.QuickCheck
import Test.Repa
import Test.Utils


propDegRadInvertible :: Property
propDegRadInvertible = 
    forAll genRepaUnboxedArray (\xs ->
        computeS (toDeg (toRad xs)) =~ xs)


propRadDegInvertible :: Property
propRadDegInvertible =
    forAll genRepaUnboxedArray (\xs ->
        computeS (toRad (toDeg xs)) =~ xs)
