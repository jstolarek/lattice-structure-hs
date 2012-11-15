{-# LANGUAGE FlexibleContexts, BangPatterns #-}
module Signal.Wavelet.Repa.Common where

import Control.Monad.Identity (runIdentity)
import Data.Array.Repa as R
import Data.Array.Repa.Unsafe (unsafeBackpermute)


{-# INLINE forceP #-}
forceP :: Array D DIM1 Double 
       -> Array U DIM1 Double
forceP = runIdentity . computeP


{-# INLINE forceS #-}
forceS :: Array D DIM1 Double 
       -> Array U DIM1 Double
forceS = computeS


{-# INLINE toDeg #-}
toDeg :: (Source r Double) 
      => Array r DIM1 Double 
      -> Array D DIM1 Double
toDeg = R.map (\x -> x * 180 / pi)


{-# INLINE toRad #-}
toRad :: (Source r Double) 
      => Array r DIM1 Double 
      -> Array D DIM1 Double
toRad = R.map (\x -> x * pi / 180)


{-# INLINE inv #-}
inv :: (Source r Double)
    => Array r DIM1 Double 
    -> Array D DIM1 Double
inv xs = unsafeBackpermute ext reversedIndex xs
    where
      reversedIndex (Z :. i) = Z :. (sh - i - 1)
      ext = extent xs
      sh  = size ext
