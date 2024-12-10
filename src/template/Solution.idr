module Main

import Data.List
import Data.Maybe
import Data.String

import Debug.Trace

import System.File

import Text.PrettyPrint.Bernardy

import Lib

%default total

FILENAME : String
FILENAME = "./inputs/d00.txt"

sol1 : String -> ?sol1ty
sol1 _ = 1

sol2 : String -> ?sol2ty
sol2 _ = 2

ex1 : String
ex1 = """
"""

ex2 : String
ex2 = ex1

opts : LayoutOpts
opts = Opts 60

export
partial
run1 : IO()
run1 = putStrLn $ Doc.render opts $ pretty $ sol1 ex1
-- run1 = do file <- readFile FILENAME
--           case file of
--                Right line => printLn $ sol1 line
--                Left _ => putStrLn "Error reading file"

export
partial
run2 : IO()
run2 = putStrLn $ Doc.render opts $ pretty $ sol2 ex2
-- run2 = do file <- readFile FILENAME
--           case file of
--                Right line => printLn $ sol2 line
--                Left _ => putStrLn "Error reading file"
