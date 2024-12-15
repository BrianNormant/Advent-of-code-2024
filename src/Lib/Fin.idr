module Lib.Fin

import Data.Fin

%default total

export
||| Try to convert a Nat to a Valid Fin
||| Can be used to safeIndex with index', replaceAt', ect
tryFin : (n : Nat) -> Nat -> Maybe (Fin n)
tryFin Z _ = Nothing
tryFin (S Z) (S Z) = Nothing
tryFin (S _) Z = Just FZ
tryFin (S n) (S k) = map FS (tryFin n k)

export
inc : {n:Nat} -> Fin n -> Maybe (Fin n)
inc x = strengthen (FS x)

export
shift1 : Fin n -> Fin (S n)
shift1 x = FS x

export
dec : {n:Nat} -> Fin n -> Maybe (Fin n)
dec FZ = Nothing
dec (FS x) = Just $ weaken x

export
finToInt : Fin _ -> Int
finToInt f = cast $ finToNat f
