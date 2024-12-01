module Main

import Data.String
import Data.List
import Data.List1
import Debug.Trace

import System.File

%default total

export infixr 1 ||>

||| Pipeline style function composition.
||| if $ is the applied form of .
||| then |> is the applied form of ||>
export
(||>) : (a -> b) -> (b -> c) -> a -> c
f ||> g = g . f

FILENAME : String
FILENAME = "./inputs/d01.txt"


parseLine : String -> List Integer
parseLine = split (== ' ')
           ||> forget
           ||> mapMaybe parseInteger

sol1 : String -> ?sol1ty
sol1 s = s |> lines
           |> map parseLine
           |> transpose
           |> map sort
           |> apply (\l => case l of
                    [l1, l2] => Just (zip l1 l2)
                    _ => Nothing
                    )
           |> map (map (\(a,b) => abs (a - b)))
           |> map sum

maybePair : List a -> Maybe (a, a)
maybePair [x, y] = Just (x, y)
maybePair _ = Nothing

sol2 : String -> ?sol2ty
sol2 s = s |> lines
           |> map parseLine
           |> transpose
           |> maybePair
           |> map ( fn ||> sum )
           where fn : (List Integer, List Integer) -> List Integer
                 fn (l1, l2) = map (\x =>
                               l2 |> filter (== x)
                                  |> length |> natToInteger |> (* x)
                                               ) l1

ex1 : String
ex1 = """
3   4
4   3
2   5
1   3
3   9
3   3
"""

ex2 : String
ex2 = ex1

export
partial
run1 : IO()
--run1 = printLn $ show $ sol1 ex1
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
