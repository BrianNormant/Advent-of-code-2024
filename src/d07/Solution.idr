module Main

import Text.Lexer
import Text.Parser
import Data.String
import Data.List
import Data.Maybe
import Data.Either
import Debug.Trace

import System.File

import Lib

%default total

FILENAME : String
FILENAME = "./inputs/d07.txt"

data EqKind = Number
            | Colon
            | Space

Eq EqKind where
  Number == Number = True
  Colon == Colon = True
  Space == Space = True
  _ == _ = False

TokenKind EqKind where
  TokType Number = Integer
  TokType Colon = ()
  TokType Space = ()

  tokValue Number x = cast x
  tokValue Colon _ = ()
  tokValue Space _ = ()

EqToken : Type
EqToken = Token EqKind

tmap : TokenMap EqToken
tmap = [
  (digits, Tok Number),
  (is ':', Tok Colon),
  (is ' ', Tok Space)
  ]

eqGrammar : Grammar state EqToken True (Integer, List1 Integer)
eqGrammar = do n <- match Number
               match Colon
               match Space
               l <- sepBy1 (match Space) (match Number)
               pure (n, l)


-- memoize this function in case performance is an issue
partial
process : List Integer -> List Integer
process [] = []
process [x] = [x]
process (x::y::xs) = (process ((x * y) :: xs)) ++ (process ((x + y) :: xs))

partial
sol1 : String -> ?sol1ty
sol1 = lines
   ||> map (lex tmap
        ||> fst
        ||> parse eqGrammar
        ||> getRight
        ||> map (fst
             ||> apply (\(i, l) =>
                  (process (forget l)
                  |> filter (== i)
                  |> length
                  |> (== Z)) <|> (0, i)
                  )
             )
        )
   ||> catMaybes
   ||> sum

(||) : Integer -> Integer -> Integer
a || b = parseInteger ((show a) ++ (show b))
      |> fromMaybe 0

partial
process2 : List Integer -> List Integer
process2 [] = []
process2 [x] = [x]
process2 (x::y::xs) = (process2 ((x * y) :: xs))
                   ++ (process2 ((x + y) :: xs))
                   ++ (process2 ((x || y) :: xs))



partial
sol2 : String -> ?sol2ty
sol2 = lines
   ||> map (lex tmap
        ||> fst
        ||> parse eqGrammar
        ||> getRight
        ||> map (fst
             ||> apply (\(i, l) =>
                  (process2 (forget l)
                  |> filter (== i)
                  |> length
                  |> (== Z)) <|> (0, i)
                  )
             )
        )
   ||> catMaybes
   ||> sum

ex1 : String
ex1 = """
190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20
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
