module Lib.List

import Data.List
import Data.Fin
import Data.Integral

%default total

||| Same as index'
||| Construct a new list consisting of all but the indicated element.
||| But use Fin instead of a proof
export
deleteAt' : (xs : List a) -> Fin (length xs) -> List a
deleteAt' (_::xs) FZ = xs
deleteAt' (x::xs) (FS i) = x :: ( deleteAt' xs i )

||| Same as index'
||| Replace an element at a particlar index with another.
||| But uses Fin instead of a proof
export
replaceAt' : (xs : List a) -> Fin (length xs) -> a -> List a
replaceAt' (_ :: xs) FZ y = y :: xs
replaceAt' (x :: xs) (FS k) y = x :: replaceAt' xs k y

||| Same as index'
||| Insert an element at a particular index.
||| But uses Fin instead of a proof
export
insertAt' : (l : List a) -> Fin (length l) -> a -> List a
insertAt' [] _ a impossible
insertAt' (x::xs) FZ a = a :: x :: xs
insertAt' (x::xs) (FS k) a = x :: insertAt' xs k a

||| Same as index'
||| Insert an element just after a particular index.
||| But uses Fin instead of a proof
export
insertAfterAt' : (l : List a) -> Fin (length l) -> a -> List a
insertAfterAt' [] _ a impossible
insertAfterAt' (x::xs) FZ a = a :: x :: xs
insertAfterAt' (x::xs) (FS k) a = x :: insertAfterAt' xs k a

||| try to index a List, if the index is out of bound return Nothing
export
indexMaybe : Nat -> List a -> Maybe a
indexMaybe Z (x :: _) = Just x
indexMaybe (S i) (_::xs) = indexMaybe i xs
indexMaybe _ [] = Nothing

export
||| take the middle element of list
middle : List a -> Maybe a
middle [] = Nothing
middle [x] = Just x
middle l = if (odd (length l))
              then indexMaybe ((length l) `div` 2) l
              else Nothing

export
||| swap to element by their index in a List
||| @arg l list to swap on
||| @arg idx1, idx2 index to swap
swapAt' : (l : List a) -> Fin (length l) -> Fin (length l) -> List a
swapAt' [] _ _ = []
swapAt' l x y = let a = index' l x
                    b = index' l y
                 in replaceAt' l x b
                 --- I'd need a proof that the lenght of the original
                 --- list is the same that the lenght of the intermediary list
                 |> (\l => replaceAt' l (believe_me y) a)

export
||| swap the first element of a list that respect a predicate
||| with a different element
||| ex: swap (== 0) [1,0,3,4] 9 := [1,9,3,4]
swapIf : (a -> Bool) -> a -> List a -> List a
swapIf _ _ [] = []
swapIf p a (x::xs) with (p x)
  _ | True = a :: xs
  _ | False = x :: (swapIf p a xs)

export
splitPairs : List (a, b) -> (List a, List b)
splitPairs [] = ([], [])
splitPairs ((a, b)::t) = let (xs, ys) = splitPairs t in (a ::xs, b :: ys)

export
mapif : (a -> Bool) -> (a -> a) -> List a -> List a
mapif _ _ [] = []
mapif p f (x::xs) = (if (p x) then f x else x ) :: mapif p f xs
