module Lib.Vect

import Data.Vect
import Data.List

import System
import Debug.Trace

%default total

||| Remove the last element of a Vect
export
dropLast : Vect (S n) a -> Vect n a
dropLast = reverse . tail . reverse

export
toVect' : (n : Nat) -> List a -> Maybe (Vect n a)
toVect' Z [] = Just []
toVect' Z _ = Just []
toVect' (S n) [] = Nothing
toVect' (S n) (x::xs) = do v <- toVect' n xs
                           pure (x::v)

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

export
toMatS : (n : Nat) -> List (List a) -> Maybe (Vect n (Vect n a))
toMatS n l = l |> map (toVect n)
               |> catMaybes
               |> toVect n

export
toMatS' : (n : Nat) -> List (List a) -> Maybe (Vect n (Vect n a))
toMatS' n l = l |> map (toVect' n)
                |> catMaybes
                |> toVect' n

export
||| index a matrix with x y
indexMat : (Fin m, Fin n) -> Vect n (Vect m a) -> a
indexMat (x, y) v = index x $ index y v

export
||| replace an element in a matrix
replaceMat : (Fin m, Fin n) -> Vect n (Vect m a) -> a -> Vect n (Vect m a)
replaceMat (x, y) v a = let lin = index y v
                            lin = replaceAt x a lin
                         in replaceAt y lin v

export
||| list all the coordinates of a matrix
allCoord : {n : Nat} -> {m : Nat} -> Vect n (Vect m a) -> List (Fin m, Fin n)
allCoord {n, m} _ = bisequence ((List.allFins m), (List.allFins n))

export
elements : Vect n (Vect m a) -> List a
elements = join . map toList . toList

export
||| Starting from a point in the matrix
||| get all the contigious elements
||| ie the elements that are the same and directly next to
||| the first or similar elements
contiguous : Eq a => Show a => {n:Nat} -> Vect n (Vect n a)
         -> (Fin n, Fin n) -> List ((Fin n, Fin n), a)
contiguous v' c' = go v' c' (indexMat c' v') [] where
  ||| recursivly check if this cell is contiguous then the neighboring cells
  go : Vect n (Vect n a) -> (Fin n, Fin n) -> a
       -> List ((Fin n, Fin n), a) -> List ((Fin n, Fin n), a)
  go v c e acc with (lookup c acc)
    _ | Just _ = acc
    _ | Nothing =
      let el = indexMat c v in
          if (el == e) then
                       (let acc = (c, el) :: acc
                            nei = neighbors c v
                               |> filter ((== el) . fst)
                               |> map snd
                            acc = foldl (\acc',g =>
                                      assert_total go v g e acc'
                                      ) acc nei
                         in acc
                       )
                       else acc
