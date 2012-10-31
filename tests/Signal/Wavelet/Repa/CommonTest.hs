module Signal.Wavelet.Repa.CommonTest where

import Data.Array.Repa
import Signal.Wavelet.Repa.Common
import Test.ArbitraryInstances
import Test.Utils


propDegRadInvertible :: RepaDIM1Array -> Bool
propDegRadInvertible (RepaDIM1Array xs) = 
    computeS (toDeg (toRad xs)) =~ xs


propRadDegInvertible :: RepaDIM1Array -> Bool
propRadDegInvertible (RepaDIM1Array xs) =
    computeS (toRad (toDeg xs)) =~ xs


propDoubleLatticeInverse :: RepaDIM1Array -> Bool
propDoubleLatticeInverse (RepaDIM1Array ls) = 
    computeS (inv . inv $ ls) == ls
