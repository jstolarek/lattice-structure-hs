module Signal.WaveletTest where

import Test.QuickCheck
import Signal.Wavelet

propIdentityShift1 xs = 
    not (null xs) ==>
        shiftLeft (shiftRight xs) == id xs

propIdentityShift2 xs = 
    not (null xs) ==>
        shiftRight (shiftLeft xs) == id xs
