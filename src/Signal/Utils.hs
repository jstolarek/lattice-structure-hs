module Signal.Utils where

class AEq a where
    (~=~) :: a -> a -> Bool

instance AEq Double where
    (~=~) x y = abs ( x - y ) < (1.0e-10 :: Double)


instance (AEq a) => AEq [a] where
    (~=~) xs ys = (length xs == length ys) && 
                  (all (\(x,y) -> x ~=~ y) $ zip xs ys)
