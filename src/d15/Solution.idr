module Main

import Data.List
import Data.Maybe
import Data.String

import Derive.Prelude

import Debug.Trace

import System
import System.File

import Text.PrettyPrint.Bernardy

import Control.Monad.State

import IdrisGL
import IdrisGL.Color as Color

import Lib

%default total
%language ElabReflection


FILENAME : String
FILENAME = "./inputs/d15.txt"

sol1 : String -> ?sol1ty
sol1 _ = 1

sol2 : String -> ?sol2ty
sol2 _ = 2

ex1 : String
ex1 = """
##########
#..O..O.O#
#......O.#
#.OO..O.O#
#..O@..O.#
#O#..O...#
#O..O..O.#
#.OO.O.OO#
#....O...#
##########

<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<
"""

ex2 : String
ex2 = """
########
#..O.O.#
##@.O..#
#...O..#
#.#.O..#
#...O..#
#......#
########

<^^>>>vv<v>>v<<
"""

opts : LayoutOpts
opts = Opts 60

-- window definition
SCREENX : Int
SCREENX = 3840
SCREENY : Int
SCREENY = 2160
FPS : Double
FPS = 144.0
RESX : Int
RESX = 1000
RESY : Int
RESY = 1000

||| fit as many block as possible
DIMBLOCK : Int -> Int
DIMBLOCK sizeX = div RESX sizeX

newWindow : Display
newWindow = InWindow "Advent of Code Day 15"
                     (MkRect (div SCREENX 2 - div RESX 2 )
                             (div SCREENY 2 - div RESY 2 )
                             RESX
                             RESY
                     )

data Tile = Empty | Wall | Block
tileColor : Tile -> Color
tileColor Empty = Color.black
tileColor Wall = Color.white
tileColor Block = Color.green

Map : Nat -> Type
Map n = Vect n (Vect n ((Fin n, Fin n), Tile))

Robot : Nat -> Type
Robot n = (Fin n, Fin n)
data Command = CUp | CDown | CLeft | CRight
%runElab derive "Command" [Eq, Show]

inverse : Command -> Command
inverse CUp = CDown
inverse CDown = CUp
inverse CLeft = CRight
inverse CRight = CLeft

data ParseError = PENoRobot
                | PEInstrInvalid
                | PEEmptyMap

%runElab derive "ParseError" [Show]

record Model where
     constructor MkModel
     size : Nat
     map : Map size
     instr : List Command
     robot : Robot size

parseMap : List (List Char) -> Either ParseError (s ** (Robot s, Map s))
parseMap [] = Left PEEmptyMap
parseMap m = let (s ** v) = (toVectD m)
                 m = map (map (\c =>
                        case c of
                             '#' => Wall
                             'O' => Block
                             _ => Empty
                        )) v
                  |> zipWithIndexMat
                 r = matFind (\(_, c) => c == '@') (zipWithIndexMat v)
                  |> map (\(c,_) => c)
                  -- |> map (\r => (s ** (r, m)))
              in case r of
                      Just r => Right (s ** (r, m))
                      Nothing => Left PENoRobot

parseInstr : String -> Either ParseError (List Command)
parseInstr = unpack ||> traverse (\c => case c of
                                 '<' => Right CLeft
                                 '>' => Right CRight
                                 '^' => Right CUp
                                 'v' => Right CDown
                                 _   => Left PEInstrInvalid
                                 )

parseModel : String -> Either ParseError Model
parseModel str = do
     let (map, instr) := lines str |> break (== "")
     (s ** (robot, map)) <- parseMap (Prelude.map unpack map)
     instr <- parseInstr ( joinBy "" instr )
     pure $ MkModel s map instr robot

displayRobot : { n : Nat } -> Robot n -> Picture
displayRobot (x,y) = let x = finToInt x
                         y = finToInt y
                         dim = DIMBLOCK $ cast n
                         rect = MkRect (dim * x) (dim * y) dim dim
                      in Rectangle rect Color.red True -- (div dim 10)

calc : Map n -> Int
calc = foldlMat (\s,((x,y),t) =>
       case t of
            Block => s + (finToInt x) + (100 * (finToInt y))
            _ => s
               ) 0

displayModel : StateT Model IO Picture
displayModel = do
     (MkModel size map instr r) <- get
     let w := mapMat (\(c, t) =>
             let (x, y) = bimap finToInt finToInt c
                 dim = DIMBLOCK $ cast size
                 color = tileColor t
                 rect = MkRect (x * dim) (y * dim) dim dim
              in Rectangle rect color True
             ) map |> toListMat
     let x := (displayRobot r)
     case instr of
               (i::_) => printLn i
               [] => printLn (calc map)
     -- sleep 1
     pure $ Pictures $ w ++ [x]


nextTile : {n : Nat} -> Command -> ((Fin n, Fin n) -> Maybe (Fin n, Fin n))
nextTile CUp = matUp
nextTile CDown = matDown
nextTile CLeft = matLeft
nextTile CRight = matRight

||| starting from the last wall
||| push every wall in the direction of the command
||| until we reach the "origin" position
pushB : {n' : Nat} -> Map n' -> (Fin n', Fin n') ->
        (Fin n', Fin n') -> Command -> Map n'
pushB map cc or c with (cc == or, (nextTile c) cc)
  _ | (True, Just nc) = doAtMat cc (\(c,_) => (c, Empty)) map
                     |> doAtMat nc (\(c,_) => (c, Block))
  _ | (_, Just nc) = let map = doAtMat nc (\(c,_) => (c, Block)) map
                      in case ((nextTile $ inverse c) cc) of
                              Just pc => assert_total $ pushB map pc or c
                              Nothing => map
      -- this can't ever happen
  _ | (_, _) = case ((nextTile $ inverse c) cc) of
                          Just pc => assert_total $ pushB map pc or c
                          Nothing => map

||| start from the current position and advence
||| if we hit a block/box, continue
||| if we hit a wall, just stop
||| if we hit a empty space, push boxes into it, then move the robot
||| if we hit the map border, stop
tryMB : {n : Nat} -> Map n -> (Fin n, Fin n) -> Robot n -> Command -> (Map n, Robot n)
tryMB map cur org cmd with ((nextTile cmd) cur)
  _ | (Just nc) with (indexMat nc map)
       _ | (_, Block) = assert_total tryMB map nc org cmd
       _ | (_, Wall)  = (map, org)
       _ | (_, Empty) = let g = (nextTile cmd) org
                         in case g of
                                 Just lst => (pushB map cur lst cmd, lst)
                                 Nothing => (map, org)
  _ | Nothing = (map, org)



||| try to make the robot move
||| succeed if the next case is empty
||| or if the next case is a box that can be pushed
move : {n : Nat} -> Map n -> Robot n -> Command -> (Map n, Robot n)
move {n=Z} map robot _ = (map, robot)
move {n=S _} map robot@(x,y) c =
     let nt = (nextTile c) robot
              |> maybe ((FZ, FZ), Wall) (\x => indexMat x map)
      in case nt of
              (_, Wall)   => (map, robot)
              (nr, Empty) => (map, nr)
              (nr, Block) => tryMB map robot robot c

partial
simModel : Double -> StateT Model IO ()
simModel _ = modify fn where
     fn : Model -> Model
     fn (MkModel size map (i::is) robot) = let (m, r) := move map robot i in
                                               MkModel size m is r
     fn m = m
-- simModel _ = modify id

export
partial
run1 : IO()
-- run1 = do let line := ex1
--           let window := newWindow
--           let fps := (1.0 / FPS)
--           let model := parseModel line
--           case model of
--                Right m => simulateStateT window Color.black fps m displayModel simModel
--                Left err => printLn err
run1 = do Right line <- readFile FILENAME
               | Left err => printLn err
          let window := newWindow
          let fps := (1.0 / FPS)
          let model := parseModel line
          case model of
               Right m => simulateStateT window Color.black fps m displayModel simModel
               Left err => printLn err

-- run1 = putStrLn $ Doc.render opts $ pretty $ sol1 ex1
-- run1 = do file <- readFile FILENAME
--           case file of
--                Right line => putStrLn $ Doc.render opts $ pretty $ sol1 line
--                Left _ => putStrLn "Error reading file"

export
partial
run2 : IO()
-- run2 = putStrLn $ Doc.render opts $ pretty $ sol2 ex2
run2 = do file <- readFile FILENAME
          case file of
               Right line => putStrLn $ Doc.render opts $ pretty $ sol2 line
               Left _ => putStrLn "Error reading file"
