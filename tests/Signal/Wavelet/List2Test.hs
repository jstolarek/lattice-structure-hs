module Signal.Wavelet.List2Test where

import qualified Signal.Wavelet.List1 as L1
import Signal.Wavelet.List.Common (inv)
import Signal.Wavelet.List2
import Test.ArbitraryInstances    (DwtInputList(..))
import qualified Test.Data.Wavelet as DW
import Test.HUnit                 (Assertion)
import Test.Utils                 ((=~), (@=~?))


testDwt :: ([Double], [Double], [Double]) -> Assertion
testDwt (ls, sig, expected) = 
    expected @=~? dwt ls sig


dataDwt :: [([Double], [Double], [Double])] 
dataDwt = DW.dataDwt


testIdwt :: ([Double], [Double], [Double]) -> Assertion
testIdwt (ls, sig, expected) = 
    expected @=~? idwt ls sig


dataIdwt :: [([Double], [Double], [Double])] 
dataIdwt = DW.dataIdwt


propDWTInvertible :: DwtInputList -> Bool
propDWTInvertible (DwtInputList (ls, sig)) = 
    idwt (inv ls) (dwt ls sig) =~ sig


propDWTIdenticalToList1 :: DwtInputList -> Bool
propDWTIdenticalToList1 (DwtInputList (ls, sig)) = 
    L1.dwt ls sig =~ dwt ls sig


propIDWTIdenticalToList1 :: DwtInputList -> Bool
propIDWTIdenticalToList1 (DwtInputList (ls, sig)) = 
    L1.idwt ls sig =~ idwt ls sig
