{-# LANGUAGE FlexibleContexts #-}

module Signal.Repa.Wavelet where

import Data.Array.Repa as R

dwtR :: (Source r0 Double) => Array r0 DIM1 Double -> Array D DIM1 Double -> Array D DIM1 Double
dwtR angles signal = go layers signal
    where
      go 0 sig        = cyclicShiftRightR sig
      go n sig        = go (n-1) (doLayer sig (weights ! (Z :. (layers - n))))
      doLayer sig wei = cyclicShiftLeftR . latticeLayerR wei $ sig
      weights         = anglesToWeightsR angles
      layers          = size . extent $ angles

idwtR :: (Source r0 Double) => Array r0 DIM1 Double -> Array D DIM1 Double -> Array D DIM1 Double
idwtR angles signal = go layers signal
    where
      go 0 sig        = cyclicShiftLeftR sig
      go n sig        = go (n-1) (doLayer sig (weights ! (Z :. (layers - n))))
      doLayer sig wei = cyclicShiftRightR . latticeLayerR wei $ sig
      weights         = anglesToWeightsR angles
      layers          = size . extent $ angles

latticeLayerR :: (Source r Double) 
                => (Double, Double) 
                -> Array r DIM1 Double
                -> Array D DIM1 Double
latticeLayerR (s, c) xs = fromPairsR . R.map baseOp . toPairsR $ xs
    where
      baseOp (x1, x2) = (x1 * c + x2 * s,  x1 * s - x2 * c )
                                 
toPairsR :: (Source r Double) 
           => Array r DIM1 Double 
           -> Array D DIM1 (Double, Double)
toPairsR xs = traverse xs twiceShorter wrapPairs
    where
      twiceShorter (Z :. s) = (Z :. s `div` 2)
      wrapPairs f  (Z :. i) = ( f ( Z :. 2 * i ), f ( Z :. 2 * i + 1))

fromPairsR :: (Source r (Double,Double))
             => Array r DIM1 (Double, Double)
             -> Array D DIM1 Double
fromPairsR xs = traverse xs twiceLonger unwrapPairs
    where
      twiceLonger   (Z :. s) = (Z :. 2 * s)
      unwrapPairs f (Z :. i) 
                      | even (i `mod` 2) = fst .f $ ( Z :. i `div` 2)
                      | otherwise        = snd .f $ ( Z :. i `div` 2)

-- smallness hint?
anglesToWeightsR :: (Source r Double) => Array r DIM1 Double -> Array D DIM1 (Double, Double)
anglesToWeightsR = R.map (\x -> (sin x, cos x))

invLSR :: (Source r Double) => Array r DIM1 Double -> Array D DIM1 Double
invLSR xs = backpermute ext reversedIndex xs
    where
      reversedIndex (Z :. i) = (Z :. (sh - i - 1))
      ext = extent xs
      sh = size ext

cyclicShiftLeftR :: (Source r Double) => Array r DIM1 Double -> Array D DIM1 Double
cyclicShiftLeftR xs = backpermute ext shift xs
    where
      shift (Z :. i) = if i /= (sh-1) 
                       then (Z :. (i + 1))
                       else (Z :. 0)
      ext = extent xs
      sh = size ext

cyclicShiftRightR :: (Source r Double) => Array r DIM1 Double -> Array D DIM1 Double
cyclicShiftRightR xs = backpermute ext shift xs
    where
      shift (Z :. 0) = (Z :. (sh-1))
      shift (Z :. i) = (Z :. (i-1))
      ext = extent xs
      sh = size ext

toDegR :: (Source r Double) => Array r DIM1 Double -> Array D DIM1 Double
toDegR = R.map (\x -> x * 180 / pi)

toRadR :: (Source r Double) => Array r DIM1 Double -> Array D DIM1 Double
toRadR = R.map (\x -> x * pi / 180)
