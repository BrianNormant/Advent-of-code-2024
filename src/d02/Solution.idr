module Main

import Data.String
import Data.List
import Data.List.Lazy
import Data.List1
import Data.Fin
import Debug.Trace

import System.File
import Lib

%default total

FILENAME : String
FILENAME = "./inputs/d02.txt"

parseLine : String -> List Integer
parseLine = String.split (== ' ')
        ||> forget
        ||> List.mapMaybe parseInteger

isSafe : List Integer -> Bool
isSafe l = let inc = delay $ all (>= 0) l
               dec = delay $ all (<= 0) l
               sep = delay $ all (\i => i >= 1 && i <= 3) (map abs l)
            in (inc || dec) && sep

sol1 : String -> ?sol1ty
sol1 = lines
   ||> map (
     parseLine
     ||> (\l => zip l (fromMaybe [] (tail' l)))
     ||> map (uncurry (-))
     )
   ||> filter isSafe
   ||> length

||| Return the list itself and all the list which can be created
||| removing each element.
||| ex: [1,2,3,4] -> [ [1,2,3,4], [2,3,4], [1,3,4], [1,2,4], [1,2,3] ]
fn : List a -> LazyList (List a)
fn [] = []
fn xs = xs :: ( map (\i => Lib.deleteAt' xs i ) (fromList $ List.allFins (length xs)) )
--- example of Lazy evaluation, the subLists will only be computed if needed,
--- whereas in the non-lazy solution, every single sublist will be created even if not necessary
-- fn : Show a => List a -> List (List a)
-- fn [] = []
-- fn xs = xs :: ( map (\i => Lib.deleteAt' xs i |> traceVal ) (List.allFins (length xs)) )

sol2 : String -> ?sol2ty
sol2 = lines
   ||> map (
     parseLine
     ||> fn
     ||> map (
       (\l => zip l (fromMaybe [] (tail' l)))
       ||> map (uncurry (-))
       )
     )
   ||> filter (Lazy.any isSafe)
   ||> length

ex1 : String
ex1 = """
7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9
"""

ex2 : String
ex2 = ex1

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
