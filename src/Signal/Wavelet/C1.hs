{-# LANGUAGE CPP #-}
module Signal.Wavelet.C1 where

import Control.Monad        (liftM)
import Data.Vector.Storable (Vector, unsafeToForeignPtr, unsafeFromForeignPtr0)
#if __GLASGOW_HASKELL__ > 706
import Foreign
#else
import Foreign hiding (unsafePerformIO)
#endif
import Foreign.C
import System.IO.Unsafe     (unsafePerformIO)

foreign import ccall unsafe "dwt.h"
  c_dwt  :: Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO (Ptr CDouble)
foreign import ccall unsafe "dwt.h"
  c_idwt :: Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO (Ptr CDouble)
foreign import ccall unsafe "dwt.h"
  c_lattice :: CInt -> CDouble -> CDouble -> Ptr CDouble -> CInt ->
               IO (Ptr CDouble)


dwt :: Vector Double -> Vector Double -> Vector Double
dwt ls sig = dwtWorker c_dwt ls sig


idwt :: Vector Double -> Vector Double -> Vector Double
idwt ls sig = dwtWorker c_idwt ls sig


dwtWorker :: (Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> IO (Ptr CDouble))
          -> Vector Double
          -> Vector Double
          -> Vector Double
dwtWorker dwtFun ls sig = unsafePerformIO $ do
    let (fpLs , _, lenLs ) = unsafeToForeignPtr ls
        (fpSig, _, lenSig) = unsafeToForeignPtr sig
    pDwt <- liftM castPtr $ withForeignPtr fpLs $ \ptrLs ->
            withForeignPtr fpSig $ \ptrSig ->
                dwtFun (castPtr ptrLs ) (fromIntegral lenLs )
                       (castPtr ptrSig) (fromIntegral lenSig)
    fpDwt <- newForeignPtr finalizerFree pDwt
    return $ unsafeFromForeignPtr0 fpDwt lenSig


lattice :: Int
        -> (Double, Double)
        -> Vector Double
        -> Vector Double
lattice !layerModifier !(!sin_, !cos_) sig = unsafePerformIO $ do
    let (fpSig, _, lenSig) = unsafeToForeignPtr sig
    pLattice <- liftM castPtr $ withForeignPtr fpSig $ \ptrSig ->
                c_lattice (fromIntegral layerModifier)
                          (realToFrac sin_) (realToFrac cos_)
                          (castPtr ptrSig ) (fromIntegral lenSig)
    fpLattice <- newForeignPtr finalizerFree pLattice
    return $ unsafeFromForeignPtr0 fpLattice lenSig
