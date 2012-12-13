{-# LANGUAGE TypeFamilies #-}
module Signal.Wavelet.Repa.Common where

import Control.Monad.Identity (runIdentity)
import Data.Array.Repa        as R
import Data.Array.Repa.Eval   (Load, Target)
import Data.Array.Repa.Unsafe as R (unsafeBackpermute)


{-# INLINE forceP #-}
forceP :: (Load r DIM1 e, Target U e, Source U e)
       => Array r DIM1 e 
       -> Array U DIM1 e
forceP = runIdentity . computeP


{-# INLINE forceS #-}
forceS :: (Load r DIM1 e, Target U e)
       => Array r DIM1 e 
       -> Array U DIM1 e
forceS = computeS


{-# INLINE fromUtoD #-}
fromUtoD :: (Source r Double, r ~ U) 
         => (Array r DIM1 Double -> Array D DIM1 Double)
         -> (Array U DIM1 Double -> Array D DIM1 Double)
fromUtoD = id


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
inv !xs = R.unsafeBackpermute ext reversedIndex xs
    where
      {-# INLINE reversedIndex #-}
      reversedIndex !(Z :. i) = Z :. (sh - i - 1)
      !ext = extent xs
      !sh  = size ext
