module Signal.Wavelet.Repa1Bench where

import Control.Arrow   ((&&&))
import Data.Array.Repa

import Signal.Wavelet.Repa1
import Signal.Wavelet.Repa.Common (forceS, forceP)


{-# INLINE benchDwtS #-}
benchDwtS :: (Array U DIM1 Double, Array U DIM1 Double) 
          -> Array U DIM1 Double
benchDwtS = uncurry dwtS


{-# INLINE benchDwtP #-}
benchDwtP :: (Array U DIM1 Double, Array U DIM1 Double) 
          -> Array U DIM1 Double
benchDwtP = uncurry dwtP


{-# INLINE benchIdwtS #-}
benchIdwtS :: (Array U DIM1 Double, Array U DIM1 Double)
           -> Array U DIM1 Double
benchIdwtS = uncurry idwtS


{-# INLINE benchIdwtP #-}
benchIdwtP :: (Array U DIM1 Double, Array U DIM1 Double)
           -> Array U DIM1 Double
benchIdwtP = uncurry idwtP


dataDwt :: ([Double], [Double])
         -> (Array U DIM1 Double, Array U DIM1 Double)
dataDwt (ls, sig) = (fromListUnboxed (Z :. lsSize ) ls, 
                     fromListUnboxed (Z :. sigSize) sig)
    where
      lsSize  = length ls
      sigSize = length sig


{-# INLINE benchLatticeS #-}
benchLatticeS :: ((Double, Double), Array U DIM1 Double)
              -> Array U DIM1 Double
benchLatticeS = forceS . (uncurry lattice)


{-# INLINE benchLatticeP #-}
benchLatticeP :: ((Double, Double), Array U DIM1 Double)
              -> Array U DIM1 Double
benchLatticeP = forceP . (uncurry lattice)


dataLattice :: ([Double], [Double])
            -> ((Double, Double), Array U DIM1 Double)
dataLattice (ls, sig) = ((sin &&& cos) . head $ ls, 
                          fromListUnboxed (Z :. sigSize) sig)
    where sigSize = length sig


{-# INLINE benchToPairsS #-}
benchToPairsS :: Array U DIM1 Double
              -> Array U DIM1 (Double, Double)
benchToPairsS = forceS . toPairs


{-# INLINE benchToPairsP #-}
benchToPairsP :: Array U DIM1 Double
              -> Array U DIM1 (Double, Double)
benchToPairsP = forceP . toPairs


dataToPairs :: ([Double], [Double]) -> Array U DIM1 Double
dataToPairs (_, sig) = fromListUnboxed (Z :. sigSize) sig
    where sigSize = length sig


{-# INLINE benchFromPairsS #-}
benchFromPairsS :: Array U DIM1 (Double, Double)
                -> Array U DIM1 Double
benchFromPairsS = forceS . fromPairs


{-# INLINE benchFromPairsP #-}
benchFromPairsP :: Array U DIM1 (Double, Double)
                -> Array U DIM1 Double
benchFromPairsP = forceP . fromPairs


dataFromPairs :: ([Double], [Double]) -> Array U DIM1 (Double, Double)
dataFromPairs (_, sig) = forceS . toPairs . fromListUnboxed (Z :. sigSize) $ sig
    where sigSize = length sig


{-# INLINE benchCslS #-}
benchCslS :: Array U DIM1 Double
          -> Array U DIM1 Double
benchCslS = forceS . csl


{-# INLINE benchCslP #-}
benchCslP :: Array U DIM1 Double
          -> Array U DIM1 Double
benchCslP = forceP . csl


{-# INLINE benchCslSP #-}
benchCslSP :: Array U DIM1 Double
           -> Array U DIM1 Double
benchCslSP = forceS . cslP


{-# INLINE benchCslPP #-}
benchCslPP :: Array U DIM1 Double
           -> Array U DIM1 Double
benchCslPP = forceP . cslP


{-# INLINE benchCsrS #-}
benchCsrS :: Array U DIM1 Double
          -> Array U DIM1 Double
benchCsrS = forceS . csr


{-# INLINE benchCsrP #-}
benchCsrP :: Array U DIM1 Double
          -> Array U DIM1 Double
benchCsrP = forceP . csr


{-# INLINE benchCsrSP #-}
benchCsrSP :: Array U DIM1 Double
           -> Array U DIM1 Double
benchCsrSP = forceS . csrP


{-# INLINE benchCsrPP #-}
benchCsrPP :: Array U DIM1 Double
           -> Array U DIM1 Double
benchCsrPP = forceP . csrP


dataCslCsr :: ([Double], [Double]) -> Array U DIM1 Double
dataCslCsr (_, sig) = fromListUnboxed (Z :. sigSize) sig
    where sigSize = length sig


{-# INLINE benchCslNS #-}
benchCslNS :: (Int, Array U DIM1 Double)
           -> Array U DIM1 Double
benchCslNS = forceS . (uncurry cslN)


{-# INLINE benchCslNP #-}
benchCslNP :: (Int, Array U DIM1 Double)
           -> Array U DIM1 Double
benchCslNP = forceP . (uncurry cslN)


{-# INLINE benchCsrNS #-}
benchCsrNS :: (Int, Array U DIM1 Double)
           -> Array U DIM1 Double
benchCsrNS = forceS . (uncurry csrN)


{-# INLINE benchCsrNP #-}
benchCsrNP :: (Int, Array U DIM1 Double)
           -> Array U DIM1 Double
benchCsrNP = forceP . (uncurry csrN)


dataCslNCsrN :: ([Double], [Double]) -> (Int, Array U DIM1 Double)
dataCslNCsrN (ls, sig) = (length ls, fromListUnboxed (Z :. sigSize) sig)
    where sigSize = length sig


{-# INLINE benchLatticeForceCslS #-}
benchLatticeForceCslS :: ((Double, Double), Array U DIM1 Double)
                      -> Array U DIM1 Double
benchLatticeForceCslS = forceS . csl . forceS . (uncurry lattice)


{-# INLINE benchLatticeForceCslP #-}
benchLatticeForceCslP :: ((Double, Double), Array U DIM1 Double)
                      -> Array U DIM1 Double
benchLatticeForceCslP = forceP . csl . forceP . (uncurry lattice)


{-# INLINE benchLatticeCslS #-}
benchLatticeCslS :: ((Double, Double), Array U DIM1 Double)
                 -> Array U DIM1 Double
benchLatticeCslS = forceS . csl . (uncurry lattice)


{-# INLINE benchLatticeCslP #-}
benchLatticeCslP :: ((Double, Double), Array U DIM1 Double)
                 -> Array U DIM1 Double
benchLatticeCslP = forceP . csl . (uncurry lattice)
