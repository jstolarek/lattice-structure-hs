module Signal.Wavelet.List1Test where

import Signal.Wavelet.List1
import Signal.Wavelet.List.Common (inv)
import Test.ArbitraryInstances    (DwtInputList(..))
import Test.Data.Wavelet          as DW
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
