module Main

import Data.String
import Data.List
import Data.Vect
import Debug.Trace
import Data.List.Lazy

import Lib
import System.File

%default total

FILENAME : String
FILENAME = "./inputs/d04.txt"

exMatrix : Vect 3 (Vect 3 Nat)
exMatrix = [
  [1,2,3],
  [4,5,6],
  [7,8,9]
  ]

indexMat : Vect (S n) (Vect (S n) a) -> (Fin (S n), Fin (S n)) -> a
indexMat [] _ impossible
indexMat xs (i, j) = index j $ index i xs

diagFrom : Vect n (Vect n a) -> (Fin n, Fin n) -> List a
diagFrom xy (FZ, FZ) = [indexMat xy (0, 0)]
diagFrom xy (i, FZ)  = [indexMat xy (i, 0)]
diagFrom xy (FZ, j)  = [indexMat xy (0, j)]
diagFrom xy (FS i, FS j) =
  let xy' = dropLast (map dropLast xy)
   in (indexMat xy (FS i, FS j)) :: diagFrom xy' (i, j)

diagFromRev : Vect n (Vect n a) -> (Fin n, Fin n) -> List a
diagFromRev xy = diagFrom (map reverse xy)

allViews : {n : Nat} -> Vect n (Vect n a) -> LazyList (List (List a))
allViews [] = []
allViews mat@((_::_)::_) =
  let idx = List.allFins n
         |> pair
         |> ( uncurry  permutation )
         |> filter (\(a, b) => a == last || b == last)
      -- left -> right
      l1 = delay $ matList mat
      -- right -> left
      l2 = delay $ matList $ map reverse mat
      -- top - > down
      l3 = delay $ matList $ transpose mat
      -- down -> top
      l4 = delay $ matList $ transpose $ reverse mat
      -- diags
      -- down-right -> up-left
      diax = delay $ map {f = List} (diagFrom mat) idx
      -- down-left -> up-right
      diay = delay $ map {f = List} (diagFromRev mat) idx
      -- reversed diags
      diax' = map List.reverse diax
      diay' = map List.reverse diay
   in [l1, l2, l3, l4, diax, diay, diax', diay']
   where matList : Vect m (Vect m a) -> List (List a)
         matList = toList . map toList

tmp : Vect ?a (Vect ?b Char)
tmp =
  [['M', 'M', 'M', 'S', 'X', 'X', 'M', 'A', 'S', 'M'],
  ['M', 'S', 'A', 'M', 'X', 'M', 'S', 'M', 'S', 'A'],
  ['A', 'M', 'X', 'S', 'X', 'M', 'A', 'A', 'M', 'M'],
  ['M', 'S', 'A', 'M', 'A', 'S', 'M', 'S', 'M', 'X'],
  ['X', 'M', 'A', 'S', 'A', 'M', 'X', 'A', 'M', 'M'],
  ['X', 'X', 'A', 'M', 'M', 'X', 'X', 'A', 'M', 'A'],
  ['S', 'M', 'S', 'M', 'S', 'A', 'S', 'X', 'S', 'S'],
  ['S', 'A', 'X', 'A', 'M', 'A', 'S', 'A', 'A', 'A'],
  ['M', 'A', 'M', 'M', 'M', 'X', 'M', 'M', 'M', 'M'],
  ['M', 'X', 'M', 'X', 'A', 'X', 'M', 'A', 'S', 'X']]

||| count the number of occurence
||| of the String "XMAS" is the char list
search : List Char -> Nat
search = id
     -- ||> traceVal
     ||> slidingWindows 4
     ||> Lazy.filter ((== "XMAS") . pack)
     ||> toList
     ||> length

sol1 : String -> ?sol1ty
sol1 s = let n = length (lines s)
          in (lines
         ||> map unpack
         ||> List.mapMaybe ( \x => toVect (n) x )
         ||> apply ( \x => toVect (n) x )
         ||> map ( allViews
               ||> map (map search)
               ||> toList
               ||> join
               ||> sum
         ) ) s

||| split lines as multiple sumblock of 3*3 char
splitBlock3: List String -> LazyList (List Char)
splitBlock3 xs = slidingWindows 3 xs
              |> map (map ((slidingWindows 3) . unpack))
              |> map (\c => case c of
                                 [a,b,c] => zip3 a b c
                                 _       => [] |> trace "Balls"
                                 )
              |> join
              |> map (\(a,b,c) => a ++ b ++ c)

compress : (List Char, List Char, List Char) -> String
compress (t,m,d) = pack $ t ++ m ++ d

SHOULD_MATCH : List (List (Char -> Bool))
SHOULD_MATCH = let def : List (List (Char -> Bool))
                   def = [[(== 'M'),     (const True), (== 'S')],
                          [(const True), (== 'A'),     (const True)],
                          [(== 'M'),     (const True), (== 'S')]]
                   vert  = join $ map List.reverse def
                   horz  = join $ List.transpose def
                   vehz  = join $ List.transpose $ map List.reverse def
                 in [join $ def, vert, horz, vehz]

sol2 : String -> ?sol2ty
sol2 = lines
   ||> splitBlock3
   ||> filter (\l => map ((flip zip) l) SHOULD_MATCH
               |> any (all (uncurry apply))
           )
   ||> toList
   ||> length

ex1 : String
ex1 = """
MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX
"""

ex2 : String
ex2 = """
M.SB
.A.B
M.SB
BBBB
"""

export
partial
run1 : IO()
-- run1 = printLn $ show $ sol1 ex1
run1 = do file <- readFile FILENAME
          case file of
               Right line => printLn $ sol1 line
               Left _ => putStrLn "Error reading file"

export
partial
run2 : IO()
-- run2 = printLn $ show $ sol2 ex2
run2 = do file <- readFile FILENAME
          case file of
               Right line => printLn $ sol2 line
               Left _ => putStrLn "Error reading file"
