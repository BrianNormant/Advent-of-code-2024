module Main

import Data.String
import Data.List
import Data.List.Extra
import Data.List1
import Data.Maybe
import Debug.Trace

import System.File

import Lib

%default total

FILENAME : String
FILENAME = "./inputs/d08.txt"

genAntinode : (Int, Int) -> (Int, Int) -> (Int, Int)
genAntinode (x1, y1) (x2, y2) = ((x2 - x1) * 2 + x1,
                                 (y2 - y1) * 2 + y1)

record Cell where
  constructor MkCell
  antenna : Maybe Char

Show Cell where
  show (MkCell Nothing) = "?"
  show (MkCell (Just c)) = cast c

Eq Cell where
  (MkCell (Just c)) == (MkCell (Just c')) = c == c'
  _ == _ = False

Ord Cell where
  compare (MkCell (Just c)) (MkCell (Just c')) = compare c c'
  compare (MkCell Nothing) (MkCell (Just c')) = GT
  compare (MkCell (Just c)) (MkCell Nothing) = LT
  compare (MkCell Nothing) (MkCell Nothing) = EQ

getAntennas : List (List Cell) -> List (Cell, Int, Int)
getAntennas = mapi (\j, l => mapi (
                   \i,c => (isJust (c.antenna)) <|>
                          (Just (c,cast i,cast j), Nothing )
                   ) l)
          ||> join
          ||> catMaybes
          ||> sortBy (\(c, _), (c', _) => compare c c')

process1 : List (Cell, Int, Int) -> List (Int, Int)
process1 = groupBy (\(c, _), (c', _) => c.antenna == c'.antenna)
       ||> map ( forget
         ||> combination 2
         ||> map fromVect2
         ||> map (\((_, v1), (_, v2)) => [genAntinode v1 v2,
                                          genAntinode v2 v1])
         ||> join
         )
       ||> join

parseLine : String -> Maybe ( List1 Cell )
parseLine = unpack
        ||> map (\c =>
                (c /= '.') <|> (
                  MkCell (Just c) ,
                  MkCell Nothing
                  )
                )
        ||> fromList

sol1 : String -> ?sol1ty
sol1 = lines
   ||> map parseLine
   ||> catMaybes
   ||> map forget
   ||> List1.fromList
   ||> map (apply (\l => (l |> forget |> getAntennas |> process1,
          length (List1.head l), length l))
           ||> (\(l, x, y) => filter (
               \(x', y') => (cast x') < x && (cast y') < y &&
                            x' >= 0 && y' >= 0
             ) l )
           ||> nub
           ||> length
           )
   ||> fromMaybe 0

LIMIT : Nat
LIMIT = 50

allAntinodes : (Int, Int) -> (Int, Int) -> List (Int, Int)
allAntinodes (x, y) (x', y') =
  let (dx, dy) = (x' - x, y' - y)
   in map (\n => (dx * n + x,
                  dy * n + y)
          ) [0..(cast LIMIT)]


process2 : List (Cell, Int, Int) -> List (Int, Int)
process2 = groupBy (\(c, _), (c', _) => c.antenna == c'.antenna)
       ||> map ( forget
         ||> combination 2
         ||> map fromVect2
         ||> map (\((_, v1), (_, v2)) => allAntinodes v1 v2
                                      ++ allAntinodes v2 v1)
         ||> join
         )
       ||> join

sol2 : String -> ?sol2ty
sol2 = lines
   ||> map parseLine
   ||> catMaybes
   ||> map forget
   ||> List1.fromList
   ||> map (apply (\l => (l |> forget |> getAntennas |> process2,
          length (List1.head l), length l))
           ||> (\(l, x, y) => filter (
               \(x', y') => (cast x') < x && (cast y') < y &&
                            x' >= 0 && y' >= 0
             ) l )
           ||> nub
           ||> length
           )
   ||> fromMaybe 0

ex1 : String
ex1 = """
............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............
"""

ex2 : String
ex2 = ex1

export
partial
run1 : IO()
-- run1 = printLn  $ sol1 ex1
run1 = do file <- readFile FILENAME
          case file of
               Right line => printLn $ sol1 line
               Left _ => putStrLn "Error reading file"

export
partial
run2 : IO()
-- run2 = printLn $ sol2 ex2
run2 = do file <- readFile FILENAME
          case file of
               Right line => printLn $ sol2 line
               Left _ => putStrLn "Error reading file"
