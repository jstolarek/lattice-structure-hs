module Test.ArbitraryInstances where

import Data.Array.Repa
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed  as VU
import Test.QuickCheck as QC

newtype DwtInputList = DwtInputList ([Double], [Double])
    deriving (Show)

newtype DwtInputVector = DwtInputVector (VU.Vector Double, VU.Vector Double)
    deriving (Show)

newtype DwtInputRepa = DwtInputRepa (Array U DIM1 Double, Array U DIM1 Double)
    deriving (Show)

newtype DwtInputC = DwtInputC (VS.Vector Double, VS.Vector Double)
    deriving (Show)

newtype RepaDIM1Array = RepaDIM1Array (Array U DIM1 Double)
    deriving (Show)


instance Arbitrary DwtInputList where
    arbitrary = genDwtInput >>= return . DwtInputList


instance Arbitrary DwtInputVector where
    arbitrary = do
        (ls, sig) <- genDwtInput
        return $ DwtInputVector (VU.fromList ls, VU.fromList sig)


instance Arbitrary DwtInputRepa where
    arbitrary = do
        (ls, sig) <- genDwtInput
        let lsSize  = length ls
            sigSize = length sig
        return $ DwtInputRepa (fromListUnboxed (Z :. lsSize ) ls,
                               fromListUnboxed (Z :. sigSize) sig)


instance Arbitrary RepaDIM1Array where
    arbitrary = sized $ \s -> do
        arrSize <- choose (1, s)
        list    <- vector arrSize
        return . RepaDIM1Array . fromListUnboxed ( Z :. arrSize ) $ list


instance Arbitrary DwtInputC where
    arbitrary = do
        (ls, sig) <- genDwtInput
        return $ DwtInputC (VS.fromList ls, VS.fromList sig)


genDwtInput ::QC.Gen ([Double],[Double])
genDwtInput = QC.sized $ \s -> do
        lsSize  <- QC.choose (1, 2 + s `rem` 8)
        sigSize <- QC.choose (1 + s `mod` 4, 2 + s `mod` 2)
        ls      <- QC.vector lsSize
        sig     <- QC.vector (2 * sigSize)
        return (ls, sig)
