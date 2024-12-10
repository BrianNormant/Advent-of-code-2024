module Main

import Data.Fin
import Data.List
import Data.Maybe
import Data.String
import Data.Vect
import Debug.Trace

import Text.PrettyPrint.Bernardy

import System.File

import Lib

%default total

FILENAME : String
FILENAME = "./inputs/d10.txt"

parse1 : String -> (n ** Vect n (Vect n Bits8))
parse1 s = lines s
        |> map unpack
        |> map ( map ((+ (-48)) . (cast {to = Bits8})) )
        |> toVectD

||| get the starting points
||| ie all the 0 points
getStarting :  {n : Nat} -> Vect n (Vect n Bits8) -> List (Fin n, Fin n)
getStarting [] = []
getStarting v = let idx = List.allFins (n)
                    idx = bisequence (idx, idx)
                    -- idx = idx ++ (map swap idx)
                 in idx |> filter (\(x,y) =>
                                  (index x $ index y v) == 0
                                  )

partial
||| follow a trail from a starting point
follow : {n : Nat} -> Vect n (Vect n Bits8) -> (Fin n, Fin n)  -> List (Fin n, Fin n)
follow [] _ = []
follow v (cx, cy) with (index cx $ index cy v)
  _ | 9 = [(cx, cy)]
  _ | 0 = neighbors ((cx, cy) |> traceVal) v
       |> filter (\(p', _) => p' ==  (1))
       |> map (\(_, nc) => follow v nc)
       |> join
  _ | p = neighbors (cx, cy) v
       |> filter (\(p', _) => p' ==  (p + 1))
       |> map (\(_, nc) => follow v nc)
       |> join


opts : LayoutOpts
opts = Opts 60

partial
sol1 : String -> ?sol1ty
sol1 = parse1
   ||> (\(n' ** v) => (( v
       |> getStarting
       |> map (\c =>
              follow v c
              |> map (bimap finToNat finToNat)
              |> nub
              |> length
            )
       |> sum
       )))

sol2 : String -> ?sol2ty
sol2 _ = 2

ex1 : String
ex1 = """
89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732
"""

ex2 : String
ex2 = ex1

export
partial
run1 : IO()
-- run1 = putStrLn $ Doc.render opts $ pretty $ sol1 ex1
run1 = do file <- readFile FILENAME
          case file of
               Right line => printLn $ sol1 line
               Left _ => putStrLn "Error reading file"

export
partial
run2 : IO()
run2 = printLn $ sol2 ex2
-- run2 = do file <- readFile FILENAME
--           case file of
--                Right line => printLn $ sol2 line
--                Left _ => putStrLn "Error reading file"
