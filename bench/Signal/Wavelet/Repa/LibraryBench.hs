module Signal.Wavelet.Repa.LibraryBench where

import Data.Array.Repa as R
import System.IO.Unsafe


{-# INLINE benchComputeS #-}
benchComputeS :: Array D DIM1 Double -> Array U DIM1 Double
benchComputeS xs = computeS xs


{-# INLINE benchComputeP #-}
benchComputeP :: Array D DIM1 Double -> IO (Array U DIM1 Double)
benchComputeP xs = computeP xs


dataCompute :: ([Double], [Double]) -> Array D DIM1 Double
dataCompute = delay . f . snd


{-# INLINE benchCopyS #-}
benchCopyS :: Array U DIM1 Double -> Array U DIM1 Double
benchCopyS xs = copyS xs


{-# INLINE benchCopyP #-}
benchCopyP :: Array U DIM1 Double -> IO (Array U DIM1 Double)
benchCopyP xs = copyP xs


dataCopy :: ([Double], [Double]) -> Array U DIM1 Double
dataCopy = f . snd


{-# INLINE benchExtractS #-}
benchExtractS :: (DIM1, DIM1, Array U DIM1 Double) -> Array U DIM1 Double
benchExtractS (start, count, xs) = computeS . extract start count $ xs


{-# INLINE benchExtractP #-}
benchExtractP :: (DIM1, DIM1, Array U DIM1 Double) -> IO (Array U DIM1 Double)
benchExtractP (start, count, xs) = computeP . extract start count $ xs


dataExtract :: ([Double], [Double]) -> (DIM1, DIM1, Array U DIM1 Double)
dataExtract (ls, sig) = (Z :. 0, Z :. chunkSize, f sig)
    where chunkSize = (length ls) `quot` 2


{-# INLINE benchAppendS #-}
benchAppendS :: (Array U DIM1 Double, Array U DIM1 Double) 
             -> Array U DIM1 Double
benchAppendS (xs, ys) = computeS . append xs $ ys


{-# INLINE benchAppendP #-}
benchAppendP :: (Array U DIM1 Double, Array U DIM1 Double) 
             -> IO (Array U DIM1 Double)
benchAppendP (xs, ys) = computeP . append xs $ ys


dataAppend :: ([Double], [Double]) -> (Array U DIM1 Double, Array U DIM1 Double)
dataAppend (_, sig) = (xs, ys)
    where half = (length sig) `quot` 2
          xs   = fromListUnboxed (Z :. half) (take half sig)
          ys   = fromListUnboxed (Z :. half) (drop half sig)


{-# INLINE benchBckpermS #-}
benchBckpermS :: (DIM1, DIM1 -> DIM1, Array U DIM1 Double) 
              -> Array U DIM1 Double
benchBckpermS (sh, ext, xs) = computeS . backpermute sh ext $ xs


{-# INLINE benchBckpermP #-}
benchBckpermP :: (DIM1, DIM1 -> DIM1, Array U DIM1 Double)
              -> IO (Array U DIM1 Double)
benchBckpermP (sh, ext, xs) = computeP . backpermute sh ext $ xs


dataBckperm :: ([Double], [Double]) 
            -> (DIM1, DIM1 -> DIM1, Array U DIM1 Double)
dataBckperm (_, sig) = (Z :. len, id, arr)
    where arr = f sig
          len = size . extent $ arr


{-# INLINE benchMapS #-}
benchMapS :: (Double -> Double, Array U DIM1 Double)
          -> Array U DIM1 Double
benchMapS (f, xs) = computeS . R.map f $ xs


{-# INLINE benchMapP #-}
benchMapP :: (Double -> Double, Array U DIM1 Double)
          -> IO (Array U DIM1 Double)
benchMapP (f, xs) = computeP . R.map f $ xs


dataMap :: ([Double], [Double]) -> (Double -> Double, Array U DIM1 Double)
dataMap xs = (id, f . snd $ xs)


{-# INLINE benchTraverseS #-}
benchTraverseS :: (Array U DIM1 Double, DIM1 -> DIM1, 
                   (DIM1 -> Double) -> DIM1 -> Double) 
               -> Array U DIM1 Double
benchTraverseS (arr, ext, g) = computeS . traverse arr ext $ g


{-# INLINE benchTraverseP #-}
benchTraverseP :: (Array U DIM1 Double, DIM1 -> DIM1, 
                   (DIM1 -> Double) -> DIM1 -> Double)
               -> IO (Array U DIM1 Double)
benchTraverseP (arr, ext, g) = computeP . traverse arr ext $ g


dataTraverse :: ([Double], [Double]) 
             -> (Array U DIM1 Double, DIM1 -> DIM1, 
                 (DIM1 -> Double) -> DIM1 -> Double) 
dataTraverse xs = (f . snd $ xs, id, ($))


f :: [Double] -> Array U DIM1 Double
f xs = fromListUnboxed (Z :. (length xs)) xs
