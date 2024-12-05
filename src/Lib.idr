module Lib

import Data.Vect
import Data.List.Lazy
import Data.List
import Data.Integral

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

||| Same as index'
||| Replace an element at a particlar index with another.
||| But uses Fin instead of a proof
export
replaceAt' : (xs : List a) -> Fin (length xs) -> a -> List a
replaceAt' (_ :: xs) FZ y = y :: xs
replaceAt' (x :: xs) (FS k) y = x :: replaceAt' xs k y

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

export
pairMaybe : (Maybe a, Maybe b) -> Maybe (a, b)
pairMaybe (Just x, Just y) = Just (x, y)
pairMaybe _ = Nothing


||| try to index a List, if the index is out of bound return Nothing
export
indexMaybe : Nat -> List a -> Maybe a
indexMaybe Z (x :: _) = Just x
indexMaybe (S i) (_::xs) = indexMaybe i xs
indexMaybe _ [] = Nothing

%inline
export
||| alternative between 2 value of a pair based on a predicate
( <|> ) : Bool -> (a, a) -> a
True  <|> (a, _) = a
False <|> (_, a) = a

export
||| take the middle element of list
middle : List a -> Maybe a
middle [] = Nothing
middle [x] = Just x
middle l = if (odd (length l))
              then indexMaybe ((length l) `div` 2) l
              else Nothing

||| reaply a function until it's result doesn't change
export
partial
untilSame : Eq a => (a -> a) -> a -> a
untilSame f a = let b = f a
                 in (a == b) <|> (b, untilSame f b)

export
||| swap to element by their index in a List
||| @arg l list to swap on
||| @arg idx1, idx2 index to swap
swapAt' : (l : List a) -> Fin (length l) -> Fin (length l) -> List a
swapAt' l x y = let a = index' l x
                    b = index' l y
                 in replaceAt' l x b
                 --- I'd need a proof that the lenght of the original
                 --- list is the same that the lenght of the intermediary list
                 |> (\l => replaceAt' l (believe_me y) a)
