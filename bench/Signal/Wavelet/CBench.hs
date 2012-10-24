module Signal.Wavelet.CBench where

import Data.Vector.Storable as VS hiding (take)
import Signal.Wavelet.C
import System.Random


{-# INLINE benchDwt #-}
benchDwt :: (Vector Double, Vector Double) -> Vector Double
benchDwt (ls, sig) = dwt ls sig


{-# INLINE benchIdwt #-}
benchIdwt :: (Vector Double, Vector Double) -> Vector Double
benchIdwt (ls, sig) = idwt ls sig


dataDwt :: RandomGen g => g -> Int -> Int -> (Vector Double, Vector Double)
dataDwt gen lsSize sigSize = 
    (fromList . take  lsSize . randoms $ gen, 
     fromList . take sigSize . randoms $ gen)
