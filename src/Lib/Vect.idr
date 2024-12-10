module Lib.Vect

import Data.Vect
import Data.List

%default total

||| Remove the last element of a Vect
export
dropLast : Vect (S n) a -> Vect n a
dropLast = reverse . tail . reverse

export
indexMaybe : Nat -> Vect n a -> Maybe a
indexMaybe _ [] = Nothing
indexMaybe Z (x::_) = Just x
indexMaybe (S i) (x::xs) = indexMaybe i xs

export
||| get the direct neighbors of a cell
||| and only the vertical and horizontal ones
neighbors : {n : Nat} -> (Fin n, Fin n) -> Vect n (Vect n a) -> List (a, (Fin n, Fin n))
neighbors _ [] = []
neighbors _ [[x]] = [(x, FZ, FZ)]
neighbors {n = S (S _)} (FZ, FZ) xs = [ (index 0 xs |> index 1, 1, 0),
                                        (index 1 xs |> index 0, 0, 1) ]

neighbors {n = l@(S (S n'))} (x@(FS x'), y@(FZ)) v@(h::t) =
  let r : List (a, (Fin l, Fin l))
      r = [ (index (weaken x') $ index y v, weaken x', y) ]
      mx = strengthen (FS x)
      my = strengthen (FS y)
      addx = mx |> map (\fx => [
            (index fx $ index y v, fx, y)
                       ])
                |> fromMaybe Basics.Nil
      addy = my |> map (\fy => [
            (index x $ index fy v, x, fy)
            ])  |> fromMaybe Basics.Nil
   in r ++ addx ++ addy
neighbors {n = l@(S (S n'))} (x@(FZ), y@(FS y')) v@(h::t) =
  let r : List (a, (Fin l, Fin l))
      r = [ (index x $ index (weaken y') v, x, weaken y') ]
      mx = strengthen (FS x)
      my = strengthen (FS y)
      addx = mx |> map (\fx => [
            (index fx $ index y v, fx, y)
                       ])
                |> fromMaybe Basics.Nil
      addy = my |> map (\fy => [
            (index x $ index fy v, x, fy)
            ])  |> fromMaybe Basics.Nil
   in r ++ addx ++ addy
neighbors {n = l@(S (S n'))} (x@(FS x'), y@(FS y')) v@(h::t) =
  let r : List (a, (Fin l, Fin l))
      r = [ (index x $ index (weaken y') v, x, weaken y'),
            (index (weaken x') $ index y v, weaken x', y) ]
      mx = strengthen (FS x)
      my = strengthen (FS y)
      addx = mx |> map (\fx => [
            (index fx $ index y v, fx, y)
                       ])
                |> fromMaybe Basics.Nil
      addy = my |> map (\fy => [
            (index x $ index fy v, x, fy)
            ])  |> fromMaybe Basics.Nil
   in r ++ addx ++ addy

export
||| try to convert a matrix represented as list of list
||| to a Vect of Vect dependent with their size
||| if the size are not compatible the empty Vect is returned
toVectD : List (List a) -> (n ** Vect n (Vect n a))
toVectD [] = (_ ** [])
toVectD l = let v = l
                 |> map (toVect (length l))
                 |> catMaybes
                 |> toVect (length l)
             in maybe (_ ** []) (\v' => (_ ** v')) v
