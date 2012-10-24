module Test.Vector.Storable where

import Control.Monad (liftM2)
import Data.Vector.Storable
import Test.QuickCheck


genVector :: (Arbitrary a, Storable a) => Gen (Vector a)
genVector = listOf1 arbitrary >>= return . fromList


genVectorPair :: (Arbitrary a, Storable a, Arbitrary b, Storable b) 
                           => Gen (Vector a, Vector b)
genVectorPair = liftM2 (,) (resize 6 genVector) genVector
