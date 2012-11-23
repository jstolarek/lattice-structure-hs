module Signal.Wavelet.C1Bench where

import Control.Arrow               ((&&&))
import Data.Vector.Storable hiding (take, head)

import Signal.Wavelet.C1


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
dataLattice (ls, sig) = (0, (sin &&& cos) . head $ ls, fromList sig)
