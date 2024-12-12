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

sol2 : String -> ?sol2ty
sol2 _ = 2

ex1 : String
ex1 = """
AAAA
BBCD
BBCC
EEEC
"""

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
run2 = putStrLn $ Doc.render opts $ pretty $ sol2 ex2
-- run2 = do file <- readFile FILENAME
--           case file of
--                Right line => printLn $ sol2 line
--                Left _ => putStrLn "Error reading file"
