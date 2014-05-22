module Signal.Wavelet.Repa.CommonTest where

import Data.Array.Repa            (computeS)

import Signal.Wavelet.Repa.Common (toRad, toDeg, inv)
import Test.ArbitraryInstances    (RepaDIM1Array(..))
import Test.Utils                 ((=~))


propDegRadInvertible :: RepaDIM1Array -> Bool
propDegRadInvertible (RepaDIM1Array xs) =
    computeS (toDeg (toRad xs)) =~ xs


propRadDegInvertible :: RepaDIM1Array -> Bool
propRadDegInvertible (RepaDIM1Array xs) =
    computeS (toRad (toDeg xs)) =~ xs


propLatticeInverseInverse :: RepaDIM1Array -> Bool
propLatticeInverseInverse (RepaDIM1Array ls) =
    computeS (inv . inv $ ls) == ls
