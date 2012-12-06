module Signal.Wavelet.Repa.LibraryBench where

import Data.Array.Repa as R


benchComputeS :: Array D DIM1 Double -> Array U DIM1 Double
benchComputeS = computeS


benchComputeP :: Array D DIM1 Double -> IO (Array U DIM1 Double)
benchComputeP = computeP


dataCompute :: ([Double], [Double]) -> Array D DIM1 Double
dataCompute = delay . f . snd


benchCopyS :: Array U DIM1 Double -> Array U DIM1 Double
benchCopyS = copyS


benchCopyP :: Array U DIM1 Double -> IO (Array U DIM1 Double)
benchCopyP = copyP


dataCopy :: ([Double], [Double]) -> Array U DIM1 Double
dataCopy = f . snd


benchExtractS :: (DIM1, DIM1, Array U DIM1 Double) -> Array U DIM1 Double
benchExtractS (start, count, xs) = computeS . extract start count $ xs


benchExtractP :: (DIM1, DIM1, Array U DIM1 Double) -> IO (Array U DIM1 Double)
benchExtractP (start, count, xs) = computeP . extract start count $ xs


dataExtract :: ([Double], [Double]) -> (DIM1, DIM1, Array U DIM1 Double)
dataExtract (ls, sig) = (Z :. 0, Z :. chunkSize, f sig)
    where chunkSize = (length ls) `quot` 2


benchAppendS :: (Array U DIM1 Double, Array U DIM1 Double) 
             -> Array U DIM1 Double
benchAppendS = computeS . (uncurry append)


benchAppendP :: (Array U DIM1 Double, Array U DIM1 Double) 
             -> IO (Array U DIM1 Double)
benchAppendP = computeP . (uncurry append)


dataAppend :: ([Double], [Double]) -> (Array U DIM1 Double, Array U DIM1 Double)
dataAppend (_, sig) = (xs, ys)
    where half = (length sig) `quot` 2
          xs   = fromListUnboxed (Z :. half) (take half sig)
          ys   = fromListUnboxed (Z :. half) (drop half sig)


benchBckpermS :: (DIM1, DIM1 -> DIM1, Array U DIM1 Double) 
              -> Array U DIM1 Double
benchBckpermS (sh, ext, xs) = computeS . backpermute sh ext $ xs


benchBckpermP :: (DIM1, DIM1 -> DIM1, Array U DIM1 Double)
              -> IO (Array U DIM1 Double)
benchBckpermP (sh, ext, xs) = computeP . backpermute sh ext $ xs


dataBckperm :: ([Double], [Double]) 
            -> (DIM1, DIM1 -> DIM1, Array U DIM1 Double)
dataBckperm (_, sig) = (Z :. len, id, arr)
    where arr = f sig
          len = size . extent $ arr


benchMapS :: (Double -> Double, Array U DIM1 Double)
          -> Array U DIM1 Double
benchMapS = computeS . (uncurry R.map)


benchMapP :: (Double -> Double, Array U DIM1 Double)
          -> IO (Array U DIM1 Double)
benchMapP = computeP . (uncurry R.map)


dataMap :: ([Double], [Double]) -> (Double -> Double, Array U DIM1 Double)
dataMap xs = (id, f . snd $ xs)


benchTraverseS :: (Array U DIM1 Double, DIM1 -> DIM1, 
                   (DIM1 -> Double) -> DIM1 -> Double) 
               -> Array U DIM1 Double
benchTraverseS (arr, ext, g) = computeS . traverse arr ext $ g


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
