module Signal.Wavelet.Vector1Bench where

import Control.Arrow       ((&&&))
import Data.Vector.Unboxed (Vector, fromList)

import Signal.Wavelet.Vector1


{-# INLINE benchDwt #-}
benchDwt :: (Vector Double, Vector Double) -> Vector Double
benchDwt (ls, sig) = dwt ls sig


{-# INLINE benchIdwt #-}
benchIdwt :: (Vector Double, Vector Double) -> Vector Double
benchIdwt (ls, sig) = idwt ls sig


dataDwt :: ([Double], [Double])
        -> (Vector Double, Vector Double)
dataDwt (ls, sig) = (fromList ls, fromList sig)


{-# INLINE benchLattice #-}
benchLattice :: (Int, (Double, Double), Vector Double)
             -> Vector Double
benchLattice (lm, baseOp, sig) = lattice lm baseOp sig


dataLattice :: ([Double], [Double])
            -> (Int, (Double, Double), Vector Double)
dataLattice (ls, sig) = (1, (sin &&& cos) . Prelude.head $ ls, fromList sig)
