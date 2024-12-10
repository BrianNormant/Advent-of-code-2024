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
