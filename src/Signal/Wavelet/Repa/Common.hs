{-# LANGUAGE FlexibleContexts, BangPatterns #-}

module Signal.Wavelet.Repa.Common where

import Control.Monad.Identity
import Data.Array.Repa as R

forceP :: Array D DIM1 Double 
       -> Array U DIM1 Double
forceP = runIdentity . computeP

{-# INLINE forceP #-}

forceS :: Array D DIM1 Double 
       -> Array U DIM1 Double
forceS = computeS

{-# INLINE forceS #-}

toDeg :: (Source r Double) 
          => Array r DIM1 Double 
          -> Array D DIM1 Double
toDeg = R.map (\(!x) -> x * 180 / pi)

{-# INLINE toDeg #-}

toRad :: (Source r Double) 
          => Array r DIM1 Double 
          -> Array D DIM1 Double
toRad = R.map (\(!x) -> x * pi / 180)

{-# INLINE toRad #-}
