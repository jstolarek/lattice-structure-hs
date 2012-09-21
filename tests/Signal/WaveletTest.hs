module Signal.WaveletTest where

import Signal.Wavelet

propIdentityShift1 :: [Double] -> Bool
propIdentityShift1 xs = cyclicShiftLeft (cyclicShiftRight xs) == id xs

propIdentityShift2 :: [Double] -> Bool
propIdentityShift2 xs = cyclicShiftRight (cyclicShiftLeft xs) == id xs
