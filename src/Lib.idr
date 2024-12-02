module Lib

import Data.Fin

export infixr 1 ||>

||| Pipeline style function composition.
||| if $ is the applied form of .
||| then |> is the applied form of ||>
export
(||>) : (a -> b) -> (b -> c) -> a -> c
f ||> g = g . f



||| Same as index'
||| Construct a new list consisting of all but the indicated element.
||| But use Fin instead of a proof
export
deleteAt' : (xs : List a) -> Fin (length xs) -> List a
deleteAt' (_::xs) FZ = xs
deleteAt' (x::xs) (FS i) = x :: ( deleteAt' xs i )
