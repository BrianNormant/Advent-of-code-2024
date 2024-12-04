module Lib

import Data.Vect
import Data.List.Lazy
import Data.List

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

||| Remove the last element of a Vect
export
dropLast : Vect (S n) a -> Vect n a
dropLast = reverse . tail . reverse

||| self-explatory, create the all the permutation of a b
export
permutation : List a -> List b -> List (a,b)
permutation xx yy = concatMap (\x => map (\y => (x,y) ) yy) xx

export
pair : a -> (a, a)
pair x = (x, x)

||| example: ```
||| allNSub 3 [1,2,3,4,5,6] =
||| [[1, 2, 3], [2, 3, 4], [3, 4, 5], [4, 5, 6]]
||| ```
export
slidingWindows : Nat -> List a -> LazyList (List a)
slidingWindows n [] = []
slidingWindows n s@(_::xs) = (List.take n s) :: (
  if (length xs) < n then Nil
                     else slidingWindows n xs
                     )
