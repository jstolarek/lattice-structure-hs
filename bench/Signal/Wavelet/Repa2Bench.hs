{-# LANGUAGE FlexibleContexts #-}
module Signal.Wavelet.Repa2Bench where

import Data.Array.Repa

import Signal.Wavelet.Repa2
import Signal.Wavelet.Repa.Common (forceS, forceP)


{-# INLINE benchDwtS #-}
benchDwtS :: (Array U DIM1 Double, Array U DIM1 Double) -> Array U DIM1 Double
benchDwtS = uncurry dwtS


{-# INLINE benchDwtP #-}
benchDwtP :: (Array U DIM1 Double, Array U DIM1 Double) -> Array U DIM1 Double
benchDwtP = uncurry dwtP


{-# INLINE benchIdwtS #-}
benchIdwtS :: (Array U DIM1 Double, Array U DIM1 Double) -> Array U DIM1 Double
benchIdwtS = uncurry idwtS


{-# INLINE benchIdwtP #-}
benchIdwtP :: (Array U DIM1 Double, Array U DIM1 Double) -> Array U DIM1 Double
benchIdwtP = uncurry idwtP


{-# INLINE benchLatticeS #-}
benchLatticeS :: ((Double, Double), Array U DIM1 Double)
              -> Array U DIM1 Double
benchLatticeS = forceS . (uncurry lattice)


{-# INLINE benchLatticeP #-}
benchLatticeP :: ((Double, Double), Array U DIM1 Double)
              -> Array U DIM1 Double
benchLatticeP = forceP . (uncurry lattice)


{-# INLINE benchTrimLatticeS #-}
benchTrimLatticeS :: ((Double, Double), Array U DIM1 Double)
                  -> Array U DIM1 Double
benchTrimLatticeS = forceS . trim . (uncurry lattice)


{-# INLINE benchTrimLatticeP #-}
benchTrimLatticeP :: ((Double, Double), Array U DIM1 Double)
                  -> Array U DIM1 Double
benchTrimLatticeP = forceP . trim . (uncurry lattice)


{-# INLINE benchExtendFrontS #-}
benchExtendFrontS :: (Int, Array U DIM1 Double) -> Array U DIM1 Double
benchExtendFrontS  = forceS . (uncurry extendFront)


{-# INLINE benchExtendFrontP #-}
benchExtendFrontP :: (Int, Array U DIM1 Double) -> Array U DIM1 Double
benchExtendFrontP = forceP . (uncurry extendFront)


{-# INLINE benchExtendEndS #-}
benchExtendEndS :: (Int, Array U DIM1 Double) -> Array U DIM1 Double
benchExtendEndS = forceS . (uncurry extendEnd)


{-# INLINE benchExtendEndP #-}
benchExtendEndP :: (Int, Array U DIM1 Double) -> Array U DIM1 Double
benchExtendEndP = forceP . (uncurry extendEnd)


dataExtend :: ([Double], [Double])
           -> (Int, Array U DIM1 Double)
dataExtend (ls, sig) = (length ls, fromListUnboxed (Z :. sigSize) sig)
    where sigSize = length sig
