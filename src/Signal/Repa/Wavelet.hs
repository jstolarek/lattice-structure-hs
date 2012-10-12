{-# LANGUAGE FlexibleContexts #-}

module Signal.Repa.Wavelet where

import Data.Array.Repa as R

latticeLayer :: (Source r (Double,Double)) 
                => (Double, Double) 
                -> Array r DIM1 (Double,Double) 
                -> Array D DIM1 (Double,Double)
latticeLayer (s, c) xs = R.map baseOp xs
    where
      baseOp (x1, x2) = (x1 * c + x2 * s,  x1 * s - x2 * c )
                                 
toPairs :: (Source r Double) 
           => Array r DIM1 Double 
           -> Array D DIM1 (Double, Double)
toPairs xs = traverse xs twiceShorter wrapPairs
    where
      twiceShorter (Z :. s) = (Z :. s `div` 2)
      wrapPairs f  (Z :. i) = ( f ( Z :. 2 * i ), f ( Z :. 2 * i + 1))

fromPairs :: (Source r (Double,Double))
             => Array r DIM1 (Double, Double)
             -> Array D DIM1 Double
fromPairs xs = traverse xs twiceLonger unwrapPairs
    where
      twiceLonger   (Z :. s) = (Z :. 2 * s)
      unwrapPairs f (Z :. i) 
                      | even (i `mod` 2) = fst .f $ ( Z :. i `div` 2)
                      | otherwise        = snd .f $ ( Z :. i `div` 2)
