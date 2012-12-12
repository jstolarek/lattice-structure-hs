module Signal.Wavelet.ReferenceITest where

import Data.Array.Repa     as R
import Data.Vector.Generic as V

import qualified Signal.Wavelet.C1      as C1
import qualified Signal.Wavelet.Eval1   as E1
import qualified Signal.Wavelet.Eval2   as E2
import qualified Signal.Wavelet.List1   as L1
import qualified Signal.Wavelet.List2   as L2
import qualified Signal.Wavelet.Repa1   as R1
import qualified Signal.Wavelet.Repa2   as R2
import qualified Signal.Wavelet.Repa3   as R3
import qualified Signal.Wavelet.Vector1 as V1

import Signal.Wavelet.Eval.Common       as EC
import Signal.Wavelet.List.Common       as LC
import Signal.Wavelet.Repa.Common       (forceS)
import Test.ArbitraryInstances
import Test.Utils ((=~))


{-

Note [Verifying equivalence of all lattice implementations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
List.Common implementation of one lattice layer  is assumed to be a reference 
one. Each implementation verifies that it is identical to another one. In case 
of Repa-based algorithms only sequential implementations are tested. This 
assumes that instances of Repa's Load type class are implemented correctly. The 
following dependencies are used:


List.Common -> Eval.Common
List.Common -> Repa1 -> Repa2
List.Common -> C1 -> Vector1 -> Repa3

Read "List.Common -> C1" as "List.Commonb implementation serves as a reference 
to C1 implementation".

Note [Lattice modifier]
~~~~~~~~~~~~~~~~~~~~~~~

Implementations operating in situ (C1, Vector1 and Repa3) need one additional 
parameter compared to other implementations. This parameter takes value of 0 or 
1 and denotes the shift of base operations: 
0 - no shift
1 - one base operation wraps arounf first and last element of a signal

C1 implementation can only be compared to List implementation when there is no
shift. Vector1, C1 and Repa3 can be compared if they are passed identical shift
value.

-}

propLatticeEvalLikeList :: Double -> [Double] -> Bool
propLatticeEvalLikeList d xs = 
    LC.latticeSeq (s, c) ys =~ EC.latticePar (s, c) ys
        where (s, c) = (sin d, cos d)
              ys     = xs Prelude.++ xs


propLatticeRepa1LikeList :: DwtInputRepa -> Bool
propLatticeRepa1LikeList (DwtInputRepa (ls, sig)) = 
    LC.latticeSeq (s, c) (R.toList sig) =~ (R.toList . forceS $ 
                                             R1.lattice (s, c) sig)
        where (s, c) = (sin d, cos d)
              d      = ls R.! (Z :. 0)


propLatticeRepa2LikeRepa1 :: DwtInputRepa -> Bool
propLatticeRepa2LikeRepa1 (DwtInputRepa (ls, sig)) = 
    forceS (R1.lattice (s, c) sig) =~ forceS (R2.lattice (s, c) sig)
        where (s, c) = (sin d, cos d)
              d      = ls R.! (Z :. 0)


propLatticeC1LikeList :: DwtInputC -> Bool
propLatticeC1LikeList (DwtInputC (ls, sig)) = 
    LC.latticeSeq (s, c) (V.toList sig) =~ (V.toList $ C1.lattice 0 (s, c) sig)
        where (s, c) = (sin d, cos d)
              d      = ls V.! 0


-- See: Note [Lattice modifier]
propLatticeVector1LikeC1 :: Int -> DwtInputC -> Bool
propLatticeVector1LikeC1 a (DwtInputC (ls, sig)) = 
    (V.convert $ C1.lattice l (s, c) sig) =~ V1.lattice l (s, c) (V.convert sig)
        where (s, c) = (sin d, cos d)
              d      = ls V.! 0
              l      = abs a `rem` 2


propLatticeRepa3LikeVector1 :: Int -> DwtInputRepa -> Bool
propLatticeRepa3LikeVector1 a (DwtInputRepa (ls, sig)) = 
    V1.lattice l (s, c) vSig =~ (toUnboxed . forceS . R3.lattice l (s, c) $ sig)
        where vSig   = toUnboxed $ sig
              (s, c) = (sin d, cos d)
              d      = ls R.! (Z :. 0)
              l      = abs a `rem` 2


{-

Note [Verifying equivalence of all DWT/IDWT implementations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

List1 implementation of DWT and IDWT is assumed to be a reference one. Each 
implementation verifies that it is identical to another one. In case of 
Repa-based algorithms only sequential implementations are tested. This assumes 
that instances of Repa's Load type class are implemented correctly. The 
following dependencies are used:

List1 -> Eval1 -> List2 -> Eva2
List1 -> Repa1 -> Repa2
List1 -> C1 -> Vector1 -> Repa3

Read "List1 -> C1" as "List1 implementation serves as a reference to C1 
implementation".


Note: [Shifting input/output signal]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Implementations operating without shifts and signal extending (C1, Vector1 and 
Repa3):

a) return shifted signal in case of DWT
b) require that input signal is shofted in case of IDWT

This means that signals need to be shifted accordingly in order to compare them
with different implementation. Test dependencies are designed in such a way that
only one test requires shifting of signals (List1 -> C1). 

-}


propDWTEval1LikeList1 :: DwtInputList -> Bool
propDWTEval1LikeList1 (DwtInputList (ls, sig)) = 
    L1.dwt ls sig =~ E1.dwt ls sig


propIDWTEval1LikeList1 :: DwtInputList -> Bool
propIDWTEval1LikeList1 (DwtInputList (ls, sig)) = 
    L1.idwt ls sig =~ E1.idwt ls sig


propDWTList2LikeEval1 :: DwtInputList -> Bool
propDWTList2LikeEval1 (DwtInputList (ls, sig)) = 
    E1.dwt ls sig =~ L2.dwt ls sig


propIDWTList2LikeEval1 :: DwtInputList -> Bool
propIDWTList2LikeEval1 (DwtInputList (ls, sig)) = 
    E1.idwt ls sig =~ L2.idwt ls sig


propDWTEval2LikeList2 :: DwtInputList -> Bool
propDWTEval2LikeList2 (DwtInputList (ls, sig)) = 
    L2.dwt ls sig =~ E2.dwt ls sig


propIDWTEval2LikeList2 :: DwtInputList -> Bool
propIDWTEval2LikeList2 (DwtInputList (ls, sig)) = 
    L2.idwt ls sig =~ E2.idwt ls sig


propDWTRepa1LikeList1 :: DwtInputRepa -> Bool
propDWTRepa1LikeList1 (DwtInputRepa (ls, sig)) = 
    L1.dwt (R.toList ls) (R.toList sig) =~ (R.toList $ R1.dwtS ls sig)


propIDWTRepa1LikeList1 :: DwtInputRepa -> Bool
propIDWTRepa1LikeList1 (DwtInputRepa (ls, sig)) = 
    L1.idwt (R.toList ls) (R.toList sig) =~ (R.toList $ R1.idwtS ls sig)


propDWTRepa2LikeRepa1 :: DwtInputRepa -> Bool
propDWTRepa2LikeRepa1 (DwtInputRepa (ls, sig)) = 
    R1.dwtS ls sig =~ R2.dwtS ls sig


propIDWTRepa2LikeRepa1 :: DwtInputRepa -> Bool
propIDWTRepa2LikeRepa1 (DwtInputRepa (ls, sig)) = 
    R1.idwtS ls sig =~ R2.idwtS ls sig


-- See Note: [Shifting input/output signal]
propDWTC1LikeList1 :: DwtInputC -> Bool
propDWTC1LikeList1 (DwtInputC (ls, sig)) = 
    listDwt =~ cDwt
        where
          listDwt = L1.dwt (V.toList ls) (V.toList sig)
          cDwt    = LC.cslN (V.length ls - 1) $ V.toList (C1.dwt ls sig)


propIDWTC1LikeList1 :: DwtInputC -> Bool
propIDWTC1LikeList1 (DwtInputC (ls, sig)) = 
    listIdwt =~ cIdwt
       where
         listIdwt         = L1.idwt (V.toList ls) (V.toList sig)
         cIdwt            = V.toList . C1.idwt ls . shiftedSig ls $ sig
         shiftedSig xs ys = V.fromList (LC.csrN (V.length xs - 1) (V.toList ys))


propDWTVector1LikeC1 :: DwtInputVector -> Bool
propDWTVector1LikeC1 (DwtInputVector (ls, sig)) = 
    C1.dwt (V.convert ls) (V.convert sig) =~ (V.convert $ V1.dwt ls sig)


propIDWTVector1LikeC1 :: DwtInputVector -> Bool
propIDWTVector1LikeC1 (DwtInputVector (ls, sig)) = 
    C1.idwt (V.convert ls) (V.convert sig) =~ (V.convert $ V1.idwt ls sig)


propDWTRepa3LikeVector1 :: DwtInputRepa -> Bool
propDWTRepa3LikeVector1 (DwtInputRepa (ls, sig)) = 
    V1.dwt (toUnboxed ls) (toUnboxed sig) =~ toUnboxed (R3.dwtS ls sig)


propIDWTRepa3LikeVector1 :: DwtInputRepa -> Bool
propIDWTRepa3LikeVector1 (DwtInputRepa (ls, sig)) = 
    V1.idwt (toUnboxed ls) (toUnboxed sig) =~ toUnboxed (R3.idwtS ls sig)
