module Main

import Data.List
import Data.List.Extra
import Data.List1
import Data.Vect
import Data.Maybe
import Data.String
import Data.SortedMap

import Debug.Trace

import System.File

import Text.PrettyPrint.Bernardy

import Lib

%default total

FILENAME : String
FILENAME = "./inputs/d12.txt"

process : {n:Nat} -> Vect n (Vect n (Char, Nat))
          -> SortedMap (Char, Nat) (Int, Int) -> SortedMap (Char, Nat) (Int, Int)
process {n} mat s = allCoord mat
               |> foldl (\s,c =>
                        let nei = neighbors c mat
                            cha = indexMat c mat
                            p' = cast $ nei |> List.filter ((/= cha) . fst) |> length
                            p' = if ((length nei) /= 4)
                                    then cast $ p' + (4 - (cast $ length nei))
                                    else cast $ p'
                         in updateExisting (\(a,p) => (a+1, p+p')) cha s
                        ) s


||| identify the contiguous regions
regions : Show ty => Eq ty => Ord ty =>
          {n:Nat} -> Vect n (Vect n ty) -> Vect n (Vect n (ty, Nat))
regions mat = allCoord mat
           |> foldl (\l,c =>
                    if (isJust $ List.find (== c) (map (map fst) l |> join))
                       then l
                       else (contiguous mat c) :: l
                    ) (the (List (List ((Fin n, Fin n), ty))) [])
           |> mapi MkPair
           |> map (\(i,c) => map (\(co, ca) => (co, (ca, i))) c)
           |> foldl (\acc,c =>
                    foldl (\mat,(co, r) => replaceMat co mat r ) acc c
                    ) (map (map (\x => (x, 0))) mat)

sol1 : String -> ?sol1ty
sol1 = lines
   ||> map unpack
   ||> toVectD
   ||> (\(_ ** mat) =>
        let v = mat |> regions
            m = v |> elements
                  |> nub |> map (\k => (k, (0,0)))
                  |> SortedMap.fromList
        in process v m
       )
   ||> SortedMap.toList
   ||> map (bimap id (uncurry (*)))
   ||> map snd
   ||> sum


||| identify the contiguous regions
||| and all the points in each regions
||| and get the first point in each regions
regions' : Eq ty => Ord ty =>
          {n:Nat} -> Vect n (Vect n ty) -> (Vect n (Vect n (ty, Nat)), List (Fin n, Fin n), List (List (Fin n, Fin n)))
regions' mat = allCoord mat
           |> foldl (\l,c =>
                    if (isJust $ List.find (== c) (map (map fst) l |> join))
                       then l
                       else (contiguous mat c) :: l
                    ) (the (List (List ((Fin n, Fin n), ty))) [])
           |> apply (\lreg => ( helper mat lreg,
                    map (head' ||> map Builtin.fst) lreg |> catMaybes,
                    lreg |> map (map Builtin.fst)
                    ))
                    where
                      helper : Vect n (Vect n ty) -> List (List ((Fin n, Fin n), ty)) -> Vect n (Vect n (ty, Nat))
                      helper mat l = l
                                  |> mapi MkPair
                                  |> map (\(i,c) => map (\(co, ca) => (co, (ca, i))) c)
                                  |> foldl (\acc,c =>
                                         foldl (\mat,(co, r) => replaceMat co mat r ) acc c
                                     ) (map (map (\x => (x, 0))) mat)

process2 : Eq a => Ord a => {n : Nat} -> Vect n (Vect n a) -> List (Int, Int)
process2 mat =
  let (reg, freg, lreg) = regions' mat -- List of the first point of each regions
      fence = map (\coord =>
              let poly := poly coord reg
               in cast $ length poly
              ) freg
      areas = map (cast . length) lreg
   in zip areas fence



sol2 : String -> ?sol2ty
sol2 = lines
   ||> map unpack
   ||> toVectD
   ||> (\(_ ** mat) => process2 mat)
   ||> map (uncurry (*))
   ||> sum

ex1 : String
ex1 = """
AAAAAA
AAABBA
AAABBA
ABBAAA
ABBAAA
AAAAAA
"""

tmp : Vect 6 (Vect 6 Char)
tmp = [
  [ 'A', 'A', 'A', 'A', 'A', 'A'  ],
  [ 'A', 'A', 'A', 'B', 'B', 'A'  ],
  [ 'A', 'A', 'A', 'B', 'B', 'A'  ],
  [ 'A', 'B', 'B', 'A', 'A', 'A'  ],
  [ 'A', 'B', 'B', 'A', 'A', 'A'  ],
  [ 'A', 'A', 'A', 'A', 'A', 'A'  ]
  ]

ex2 : String
ex2 = """
RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE
"""

opts : LayoutOpts
opts = Opts 60

export
partial
run1 : IO()
-- run1 = putStrLn $ Doc.render opts $ pretty $ sol1 ex2
run1 = do file <- readFile FILENAME
          case file of
               Right line => printLn $ sol1 line
               Left _ => putStrLn "Error reading file"

export
partial
run2 : IO()
-- run2 = putStrLn $ Doc.render opts $ pretty $ sol2 ex2
run2 = do file <- readFile FILENAME
          case file of
               Right line => printLn $ sol2 line
               Left _ => putStrLn "Error reading file"
