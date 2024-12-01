module Solution

import Data.String
import Data.List
import Debug.Trace

import System.File

%default total

FILENAME : String
FILENAME = "./inputs/d01.txt"

sol1 : String -> ?sol1ty
sol1 _ = 1

sol2 : String -> ?sol2ty
sol2 _ = 2

ex1 : String
ex1 = """
"""

ex2 : String
ex2 = ex1

export
partial
run1 : IO()
run1 = printLn $ show $ sol1 ex1
-- run1 = do file <- readFile FILENAME
--           case file of
--                Right line => printLn $ sol1 line
--                Left _ => putStrLn "Error reading file"

export
partial
run2 : IO()
run2 = printLn $ show $ sol2 ex2
-- run2 = do file <- readFile FILENAME
--           case file of
--                Right line => printLn $ sol2 line
--                Left _ => putStrLn "Error reading file"
