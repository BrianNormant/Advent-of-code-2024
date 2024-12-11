module Main

import Data.Integral
import Data.List
import Data.Maybe
import Data.SortedMap
import Data.SortedMap.Dependent
import Data.String

import Debug.Trace

import System.File

import Text.PrettyPrint.Bernardy

import Lib

%default total

FILENAME : String
FILENAME = "./inputs/d11.txt"

Stone : Type
Stone = Int

blink : Stone -> List Stone
blink 0 = [1]
blink n = let str = unpack $ cast {to = String} n
              s = cast {to = Int} $ length $ str
           in case (even s) of
                   True => str |> splitAt (cast {to = Nat}(div s 2) )
                               |> pairToList
                               |> map pack
                               |> map parseInteger
                               |> catMaybes
                   False => [ n * 2024 ]

blink' : List Stone -> List Stone
blink' s = map blink s |> join

sol1 : String -> ?sol1ty
sol1 = words
   ||> map parseInteger
   ||> catMaybes
   ||> repeat 25 blink'
   ||> length

Memory : Type
-- memorise how many stones a created
-- by blinking at the same stone
-- a certain number of time
Memory = SortedMap (Nat, Stone) (Int)

process : Nat -> List Stone -> (Int, Memory)
process n l = go l n empty
            where go : List Stone -> Nat -> Memory -> (Int, Memory)
                  go (x::xs) n@(S k) mem with (lookup (n, x) mem)
                    _ | Just r = let (r', mem) = go xs n mem
                                  in (r + r', mem)
                    _ | Nothing = let r = blink x
                                      (l, mem) = go r k mem
                                      mem = insert (n, x) l mem
                                      (l', mem) = go xs n mem
                                   in (l + l', mem)
                  go [] n mem = (0, mem)
                  go l Z mem = (cast $ length l, mem)

sol2 : String -> ?sol2ty
sol2 = words
   ||> map parseInteger
   ||> catMaybes
   ||> process 75
   ||> bimap id (SortedMap.toList ||> List.length)

ex1 : String
ex1 = """
125 17
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
-- run2 = putStrLn $ Doc.render opts $ pretty $ sol2 ex2
run2 = do file <- readFile FILENAME
          case file of
               Right line => printLn $ sol2 line
               Left _ => putStrLn "Error reading file"
