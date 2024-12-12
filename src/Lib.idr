module Lib

import Data.Vect
import Data.List.Lazy
import Data.List
import Data.List1
import Data.Integral
import Data.These
import Data.List.Extra

import public Lib.Seq
import public Lib.Maybe
import public Lib.Vect
import public Lib.List
import public Lib.Fin
import public Lib.SnocList
import public Lib.Queue

export infixr 1 ||>


||| Pipeline style function composition.
||| if $ is the applied form of .
||| then |> is the applied form of ||>
export
(||>) : (a -> b) -> (b -> c) -> a -> c
f ||> g = g . f

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
||| transform a homogenous pair into a list
pairToList : (a, a) -> List a
pairToList (x, y) = [x, y]

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
maybeIntToNat : Integer -> Maybe Nat
maybeIntToNat x = if x >= 0 then Just (cast x) else Nothing


%inline
export
||| alternative between 2 value of a pair based on a predicate
( <|> ) : Bool -> (a, a) -> a
True  <|> (a, _) = a
False <|> (_, a) = a

export
repeat : Nat -> (a -> a) -> a -> a
repeat Z _ a = a
repeat (S k) f a = f (repeat k f a)

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
