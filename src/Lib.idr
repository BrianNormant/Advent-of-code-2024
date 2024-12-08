module Lib

import Data.Vect
import Data.List.Lazy
import Data.List
import Data.List1
import Data.Integral
import Data.These
import Data.List.Extra

export infixr 1 ||>


||| Pipeline style function composition.
||| if $ is the applied form of .
||| then |> is the applied form of ||>
export
(||>) : (a -> b) -> (b -> c) -> a -> c
f ||> g = g . f

export
||| Try to get a Fin n from i
tryFin : (n : Nat) -> Nat -> Maybe (Fin n)
tryFin Z _ = Nothing
tryFin (S Z) (S Z) = Nothing
tryFin (S _) Z = Just FZ
tryFin (S n) (S k) = map FS (tryFin n k)

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

||| Remove the last element of a Vect
export
dropLast : Vect (S n) a -> Vect n a
dropLast = reverse . tail . reverse

||| self-explatory, create the all the permutation of a b
export
permutation : List a -> List b -> List (a,b)
permutation xx yy = MkPair xx yy |> bisequence

export
combination : (n : Nat) -> List a -> List (Vect n a)
combination Z _ = [[]]
combination _ [] = []
combination m@(S n) (x::xs) = (map (x ::) (combination n xs))
                           ++ (combination m xs)

export
fromVect2 : Vect 2 a -> (a, a)
fromVect2 [x, y] = (x, y)

export
||| try to create a pair form a list of 2
pairFromList : List a -> Maybe (a, a)
pairFromList [x, y] = Just (x, y)
pairFromList _ = Nothing

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

export
collectJust : List (Maybe a) -> List a
collectJust [] = []
collectJust (Just x :: xs) = x :: collectJust xs
collectJust (Nothing :: xs) = collectJust xs

export
maybeIntToNat : Integer -> Maybe Nat
maybeIntToNat x = if x >= 0 then Just (cast x) else Nothing

||| try to index a List, if the index is out of bound return Nothing
export
indexMaybe : Nat -> List a -> Maybe a
indexMaybe Z (x :: _) = Just x
indexMaybe (S i) (_::xs) = indexMaybe i xs
indexMaybe _ [] = Nothing

export
indexMaybeV : Nat -> Vect n a -> Maybe a
indexMaybeV _ [] = Nothing
indexMaybeV Z (x::_) = Just x
indexMaybeV (S i) (x::xs) = indexMaybeV i xs

export
theseMaybe : (Maybe a, Maybe b) -> Maybe (These a b)
theseMaybe (Just x, Just y) = Just $ Both x y
theseMaybe (Just x, Nothing) = Just $ This x
theseMaybe (Nothing, Just y) = Just $ That y
theseMaybe (Nothing, Nothing) = Nothing

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

export
partial
||| repeat the application of a function
||| until the predicate is valid
until : (a -> Bool) -> (a -> a) -> a -> a
until p f a = let b = f a
               in p b <|> (b, until p f b)

export
total
||| limited version of until
until' : Nat -> (a -> Bool) -> (a -> a) -> a -> a
until' Z _ f a = f a
until' (S i) p f a = let b = f a
                      in p b <|> (b, until' i p f b)

||| reaply a function until it's result doesn't change
export
partial
untilSame : Eq a => (a -> a) -> a -> a
untilSame f a = let b = f a
                 in (a == b) <|> (b, untilSame f b)

||| limited version of untilSame
export
total
untilSame' : Eq a => Nat -> (a -> a) -> a -> a
untilSame' Z f a = f a
untilSame' (S i) f a = let b = f a
                        in (a == b) <|> (b, untilSame' i f b)

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
splitPairs : List (a, b) -> (List a, List b)
splitPairs [] = ([], [])
splitPairs ((a, b)::t) = let (xs, ys) = splitPairs t in (a ::xs, b :: ys)

export
mapif : (a -> Bool) -> (a -> a) -> List a -> List a
mapif _ _ [] = []
mapif p f (x::xs) = ((p x) <|> (f x, x) ) :: mapif p f xs

export
||| find the element next to given element in a list
neighbors : Eq a => a -> List a -> Maybe (These a a)
neighbors _ [] = Nothing
neighbors el xs = findIndex (== el) xs
               |> map finToInteger
               |> map (\x => (x-1, x+1))
               |> map (bimap maybeIntToNat maybeIntToNat)
               |> map pairMaybe
               |> join
               |> map (bimap ((flip indexMaybe) xs) ((flip indexMaybe) xs))
               |> map theseMaybe
               |> join

export
||| move a element from a List just before another
||| based on their index
||| [0,1,2,3,4] 3 2 -> [0,3,1,2,4]
||| [0,1,2,3,4] 3 1 -> [3,0,1,2,4]
||| [0,1,2,3,4] 3 0 -> [3,0,1,2,4]
||| @idx1 the element to move
||| @idx where to put it before
bubbleList : (l : List a) -> Fin (length l) -> Fin (length l) -> List a
bubbleList [] _ _ = []
bubbleList l@(_::_) idx FZ = (index' l idx) :: (deleteAt' l idx)
-- bubbleList [1,2,3] 0 2
bubbleList l@(x::xs) FZ (FS k) = (insertAt' xs k x)
bubbleList (x::xs) (FS k) (FS l) = x :: bubbleList xs k l

export
trySub : Nat -> Maybe Nat
trySub x with (x <= 0)
  _ | True = Nothing
  _ | False = case x of
                   Z => Nothing
                   (S k) => Just k


export
||| map each element of a matrice to its index
matWithIndex : List (List a) -> List ((Nat, Nat), a)
matWithIndex = mapi (\j,xs => mapi (\i,x => ((i, j), x)) xs)
           ||> join

-- export
-- ||| get a vector as a DPair
-- asDPair : Vect n a -> (m ** Vect m a)
-- asDPair [] = (0 ** [])
-- asDPair (x :: xs) = let (m ** rest) = asDPair xs in ((S m) ** x::rest)
--
-- export
-- forget : (m ** Vect m a) -> Vect m a
-- forget p = believe_me $ snd p
--
-- export
-- clean : (n ** Vect n (m ** Vect m a)) -> Maybe (n ** Vect n (Vect n a) )
-- clean (0 ** []) = Nothing
-- clean (n ** xs) with (xs)
--   _ | (m ** xs) = ?impl
