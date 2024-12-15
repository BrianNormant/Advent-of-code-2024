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
||| apply a function at a position in a matrix
doAtMat : (Fin m, Fin n) -> (a -> a) -> Vect n (Vect m a) -> Vect n (Vect m a)
doAtMat (x, y) f v = let old = indexMat (x, y) v
                         new = f old
                      in replaceMat (x, y) v new

export
mapMat : (a -> b) -> Vect n (Vect m a) -> Vect n (Vect m b)
mapMat f v = map (map f) v

export
foldlMat : (acc -> a -> acc) -> acc -> Vect n (Vect m a) -> acc
foldlMat f acc [] = acc
foldlMat f acc (v::vs) = foldlMat f (foldl f acc v) vs

export
toListMat : Vect n (Vect m a) -> List a
toListMat v = join $ toList $ map toList v

export
||| find an element in a matrix
matFind : {n,m : Nat} -> (a -> Bool) -> Vect n (Vect m a) -> Maybe a
matFind _ [] = Nothing
matFind f (v::vs) = case find f v of
                         Just n => Just n
                         Nothing => matFind f vs

export
indexes : {n : Nat} -> Vect n Nat
indexes {n = 0} = []
indexes {n = S k} = believe_me (fromList [0..k])

onePlusSucc : (left : Nat) -> S left = left + 1
onePlusSucc Z = Refl
onePlusSucc (S k) = cong S (onePlusSucc k)

export
zipWithIndexMat : {n,m : Nat} -> Vect n (Vect m a) -> Vect n (Vect m ((Fin m, Fin n), a))
zipWithIndexMat v = zipWith (\j,vs => zipWith (\i,a => ((i,j), a)) (allFins m) vs) (allFins n) v

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
contiguous : Eq a => {n:Nat} -> Vect n (Vect n a)
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

export
matUp : (Fin n, Fin m) -> Maybe (Fin n, Fin m)
matUp (_, FZ) = Nothing
matUp (x, (FS y)) = Just (x, weaken y)

export
matLeft : (Fin n, Fin m) -> Maybe (Fin n, Fin m)
matLeft (FZ, _) = Nothing
matLeft ((FS x), y) = Just (weaken x, y)

export
matRight : {n:Nat} -> (Fin n, Fin m) -> Maybe (Fin n, Fin m)
matRight (x,y) = map (\x' => (x', y)) (strengthen (FS x))

export
matDown : {m : Nat} -> (Fin n, Fin m) -> Maybe (Fin n, Fin m)
matDown (x,y) = map (\y' => (x, y')) (strengthen (FS y))

export
matUL : (Fin n, Fin m) -> Maybe (Fin n, Fin m)
matUL c = do c <- matUp c
             c <- matLeft c
             pure c

export
matUR : {n: Nat } -> (Fin n, Fin m) -> Maybe (Fin n, Fin m)
matUR c = do c <- matUp c
             c <- matRight c
             pure c

export
matDL : {m : Nat} -> (Fin n, Fin m) -> Maybe (Fin n, Fin m)
matDL c = do c <- matDown c
             c <- matLeft c
             pure c

export
matDR : {m : Nat} -> {n : Nat} -> (Fin n, Fin m) -> Maybe (Fin n, Fin m)
matDR c = do c <- matDown c
             c <- matRight c
             pure c

export
||| find a polygon in a 2D space
poly : Eq ty => {n:Nat} -> (Fin n, Fin n) -> Vect n (Vect n ty)
    -> List (Fin (S n), Fin (S n))
poly c mat = poly' c (indexMat c mat) mat [] |> map snd |> join where
  helper : ty -> Maybe (Fin n, Fin n) -> Bool
  helper _ Nothing = True
  helper e (Just c) = (indexMat c mat) /= e

  poly' : (Fin n, Fin n) -> ty -> Vect n (Vect n ty)
       -> List ((Fin n, Fin n), List (Fin (S n), Fin (S n)))
       -> List ((Fin n, Fin n), List (Fin (S n), Fin (S n)))
  poly' c@(x, y) e mat mem with (lookup c mem)
    _ | Just _ = mem
    _ | Nothing = if (indexMat c mat /= e)
                     then mem
                     else (
      let up    = helper e $ matUp c
          down  = helper e $ matDown c
          left  = helper e $ matLeft c
          right = helper e $ matRight c
          points = case the (List ?list) [up, down, left, right] of
                        [True, False, False, True] => [(FS x, weaken y)]
                        [True, False, True, False] => [(weaken x, weaken y)]
                        [False, True, False, True] => [(FS x, FS y)]
                        [False, True, True, False] => [(weaken x, FS y)]
                        [True, False, True,  True] =>
                          [(weaken x, weaken y), (FS x, weaken y)]
                        [True, True,  False, True] =>
                          [(FS x, weaken y), (FS x, FS y)]
                        [False, True, True,  True] =>
                          [(weaken x, FS y), (FS x, FS y)]
                        [True, True,  True, False] =>
                          [(weaken x, weaken y), (weaken x, FS y)]
                        [True, True,  True,  True] =>
                          [(weaken x, weaken y), (FS x, weaken y),
                           (weaken x, FS y), (FS x, FS y)]
                        _ => []

          ur = helper e $ matUR c
          ul = helper e $ matUL c
          dr = helper e $ matDR c
          dl = helper e $ matDL c
          points = points ++ if ur && (not up) && (not right) then [(FS x, weaken y)] else []
          points = points ++ if ul && (not up) && (not left) then [(weaken x, weaken y)] else []
          points = points ++ if dr && (not down) && (not right) then [(FS x, FS y)] else []
          points = points ++ if dl && (not down) && (not left) then [(weaken x, FS y)] else []


          mem = (c, points) :: mem
          mem = neighbors c mat
             |> filter ((== e) . fst)
             |> map snd
             |> foldl (\acc,g =>
                      assert_total poly' g e mat acc
                      ) mem
       in mem
       )
