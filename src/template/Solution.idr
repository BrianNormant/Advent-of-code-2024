module Main

import Data.List
import Data.Maybe
import Data.String

import Derive.Prelude

import Debug.Trace

import System.File

import Text.PrettyPrint.Bernardy

import Lib

%default total
%language ElabReflection


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
-- run1 = putStrLn $ Doc.render opts $ pretty $ sol1 ex1
run1 = do file <- readFile FILENAME
          case file of
               Right line => putStrLn $ Doc.render opts $ pretty $ sol1 ex1
               Left _ => putStrLn "Error reading file"

export
partial
run2 : IO()
-- run2 = putStrLn $ Doc.render opts $ pretty $ sol2 ex2
run2 = do file <- readFile FILENAME
          case file of
               Right line => putStrLn $ Doc.render opts $ pretty $ sol2 ex2
               Left _ => putStrLn "Error reading file"
