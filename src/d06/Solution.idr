module Main

import Data.String
import Data.List
import Data.These
import Data.List.Extra
import Data.Vect
import Data.Fin
import Debug.Trace

import Lib

import System.File

%default total

FILENAME : String
FILENAME = "./inputs/d06.txt"

data Orientation = Up
                 | Down
                 | Right
                 | Left

Eq Orientation where
  Up == Up = True
  Down == Down = True
  Right == Right = True
  Left == Left = True
  _ == _ = False

rotateR : Orientation -> Orientation
rotateR Up = Right
rotateR Down = Left
rotateR Right = Down
rotateR Left = Up

record Cell where
  constructor MkCell
  empty : Bool
  reached : Bool
  id : Nat

Show Cell where
  show (MkCell False True _) = "!"
  show (MkCell False False _) = "#"
  show (MkCell True True _) = "X"
  show (MkCell True False _) = "."


defCell : Nat -> Cell
defCell = MkCell False False

Map : Type
Map = List (List Cell)

exMap : Map
exMap = [
  [MkCell False False 0, MkCell False False 0, MkCell False False 0],
  [MkCell False False 0, MkCell True  False 1, MkCell False False 0],
  [MkCell False False 0, MkCell False False 0, MkCell False False 0]
  ]

record Guard where
  constructor MkGuard
  xCoord : Nat
  yCoord : Nat
  o : Orientation

Show Guard where
  show (MkGuard x y Up)    = "(" ++ show x ++ "," ++ show y ++ "):" ++ "^"
  show (MkGuard x y Down)  = "(" ++ show x ++ "," ++ show y ++ "):" ++ "v"
  show (MkGuard x y Right) = "(" ++ show x ++ "," ++ show y ++ "):" ++ ">"
  show (MkGuard x y Left)  = "(" ++ show x ++ "," ++ show y ++ "):" ++ "<"

||| Replace a position in the map with a
||| Different Cell
updateMap :  Map -> (Nat, Nat) -> Cell -> Maybe (Map)
updateMap m (x, y) c = (tryFin (length m) x, tryFin (length m) y)
                       |> pairMaybe
                       |> map (\(x, y) => index' m y
                          |> (\l => replaceAt' l (believe_me x) c)
                          |> (\l => replaceAt' m y l)
                          )


||| make the guard advance 1 step forward in the map
||| If they move in a different cell, mark it as reached
||| If they step of the map, they become nothing.
oneStep : Map -> Maybe Guard -> (Map, Maybe Guard)
oneStep m Nothing = (m, Nothing)
oneStep m (Just (MkGuard x y o)) =
  let next = the (Maybe (Cell, (Nat, Nat))) (
                  case o of
                       Up => case y of
                                  Z => Nothing
                                  (S k) => map (\c => (c, (x, k))) $ (indexMaybe k m) >>= (indexMaybe x)
                       Down => map (\c => (c, (x, S y))) $ (indexMaybe (S y) m) >>= (indexMaybe x)
                       Right => map (\c => (c, (S x, y))) $ (indexMaybe y m) >>= (indexMaybe (S x))
                       Left => case x of
                                    Z => Nothing
                                    (S k) => map (\c => (c, (k, y))) $ (indexMaybe y m) >>= (indexMaybe k)
                  )
   in map (\(c,(x',y')) =>
            case c of
                 MkCell True _ id => (
                   fromMaybe m (updateMap m (x', y') (MkCell True True id)),
                   Just $ MkGuard x' y' o
                   )
                 MkCell False  _ _ => (
                   m,
                   Just $ MkGuard x  y  (rotateR o)
                   )
          ) next |> fromMaybe (m, Nothing)

helper2 : (s : List String) -> Maybe (List (List Cell), Guard)
helper2 s = s
         |> map unpack
         |> mapi (\y,l => mapi (\x,c =>
                         case c of
                              '#' => This $ MkCell False False 1
                              '^' => Both (MkCell True True 2) (MkGuard x y Up)
                              _ =>   This $ MkCell True False 0
                         ) l )
         |> apply (\l => (
                  l |> map (map (fromThis) ||> catMaybes),
                  l |> join |> map fromThat |> catMaybes |> head'
                  ))
         |> apply (\(l, m) => case m of
                                   Just g => Just (l, g)
                                   Nothing => Nothing
                  )

sol1 : String -> ?sol1ty -- Maybe ((n ** Map n), Guard)
sol1 s = lines s
      |> helper2
      |> map (\(l, g) => (l, Just g))
      |> map (until' 10000 (\(_,g) => isNothing g) (uncurry oneStep))
      |> map ( fst ||> map (map show) ||> map (foldl (++) "") ||> joinBy "\n")
      -- |> map ( fst ||> join ||> filter (.reached) ||> List.length )
      -- |> fromMaybe 0

takeCell : (Nat, Nat) -> Map -> Maybe Cell
takeCell (x, y) = indexMaybe y ||> (=<<) (indexMaybe x)

||| fetch the next cell in a map depending of the orientation
nextCell : Map -> (Nat, Nat) -> Orientation -> Maybe Cell
nextCell m (x, S y) Up   = takeCell (x, y) m
nextCell m (S x, y) Left = takeCell (x, y) m
nextCell m (x, y) Down   = takeCell (x, S y) m
nextCell m (x, y) Right  = takeCell (S x, y) m
nextCell _ _ Left = Nothing
nextCell _ _ Up   = Nothing

nextCoord : Orientation -> (Nat, Nat) -> (Nat, Nat)
nextCoord Up    (x, S y) = (x, y)
nextCoord Left  (S x, y) = (x, y)
nextCoord Down  (x, y)   = (x, S y)
nextCoord Right (x, y)   = (S x, y)
nextCoord Up    (x, Z) = (x, Z)
nextCoord Left  (Z, y) = (Z, y)

Memory : Type
Memory = ((Int, Int), List ((Int, Int) , (Int, Int)))

inVect : (Int, Int, Orientation) -> ((Int, Int), (Int, Int)) -> Bool
inVect (x', y', o') ((x1, y1), (x2, y2)) =
  let x' = x' - x1
      y' = y' - y1
      x2 = x2 - x1
      y2 = y2 - y1
      o = if (x2 == 0) then (y2 > 0) <|> (Up, Down)
                       else (x2 > 0) <|> (Right, Left)
   in case o of
           Up => (o' == Up) && (x' == 0)
           Down => (o' == Down) && (x' == 0)
           Right => (o' == Right) && (y' == 0)
           Left => (o' == Left) && (y' == 0)

--- Update memory if a loop wasn't found
fn01 : Memory -> (Int, Int, Orientation) -> Maybe Memory
fn01 (last, mem) curr@(x, y, _) = mem
                     |> filter (inVect curr)
                     |> (\l => (length l > 0) <|> (
                       Nothing,
                       Just ((x, y), (last, (x, y)) :: mem)
                       ) )

partial
||| Iterate in the map trying to either exit or to find an infinite loop
||| Use a list of vectors as a memory to detect path if already used.
||| Return true if we found a loop
detectLoop : Map -> Memory -> Guard -> Bool
detectLoop m mem (MkGuard x y o) with (nextCell m (x, y) o)
  _ | Nothing = ( False )
  _ | (Just (MkCell True _ _)) =
    let (x', y') = nextCoord o ((x, y))
        ng = MkGuard x' y' (o)
     in detectLoop m (mem) (ng)
  _ | (Just (MkCell False _ _)) =
    let ng = MkGuard x y (rotateR o)
     in maybe True
        (\m' => detectLoop m m' ng )
        (fn01 (mem) (cast x, cast y, o))

partial
detectLoop' : Map -> Guard -> Bool
detectLoop' m g@(MkGuard x y o) = detectLoop m ((cast x, cast y), []) g


helper3 : (s : List String) -> Maybe (List (List Cell), Guard)
helper3 s = s
         |> map unpack
         |> mapi (\y,l => mapi (\x,c =>
                         case c of
                              '#' => This $ MkCell False False 1
                              '^' => Both (MkCell True False 2) (MkGuard x y Up)
                              _ =>   This $ MkCell True False 0
                         ) l )
         |> apply (\l => (
                  l |> map (map (fromThis) ||> catMaybes),
                  l |> join |> map fromThat |> catMaybes |> head'
                  ))
         |> apply (\(l, m) => case m of
                                   Just g => Just (l, g)
                                   Nothing => Nothing
                  )

partial
sol2 : String -> ?sol2ty
sol2 s = lines s
      |> helper3
      |> map (\(l, g) => ((l, g), l, Just g))
      |> map (map (until' 10000 (\(_,g) => isNothing g) (uncurry oneStep)))
      |> map (map (fst ||> matWithIndex
                       ||> filter (\(_, c) => c.reached)
             ))
      |> map (\((l, g), t) =>
               map (\(ij, c) => updateMap l ij (MkCell False False 3)) t
               |> catMaybes |> MkPair g
             )
      |> map (\(g, l) => filter (\m => detectLoop' m g) l |> MkPair g)
      |> map (\(g, l) => List.filter (\map => until' 10000 (snd ||> isNothing) (uncurry oneStep) (map, Just g) |> (snd ||> isJust) ) l)
      |> map (length)
      -- |> map ( map( map (map show) ||> map (foldl (++) "") ||> joinBy "\n"))

      -- |> map (\(l, g@(MkGuard x y _)) =>
               -- detectLoop l ((cast x, cast y), []) g
      --        )

ex1 : String
ex1 = """
....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...
"""

ex2 : String
ex2 = ex1

export
partial
run1 : IO()
run1 = printLn $ sol1 ex1
-- run1 = do file <- readFile FILENAME
--           case file of
--                Right line => printLn $ sol1 line
--                Left _ => putStrLn "Error reading file"

export
partial
run2 : IO()
-- run2 = printLn $ sol2 ex2
run2 = do file <- readFile FILENAME
          case file of
               Right line => printLn $ sol2 line
               Left _ => putStrLn "Error reading file"
