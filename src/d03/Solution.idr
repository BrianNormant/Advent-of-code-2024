module Main

import Text.Lexer
import Text.Parser
import Data.String
import Data.List
import Data.List.Lazy
import Data.List1
import Data.Either
import Data.Fin
import Debug.Trace

import System.File
import Lib

%default total

FILENAME : String
FILENAME = "./inputs/d03.txt"

data ProgKind = LParen
              | RParen
              | Comma
              | Number
              | MulIns
              | Ignore
              | Do
              | DoNot

Eq ProgKind where
  LParen == LParen = True
  RParen == RParen = True
  Comma == Comma = True
  Number == Number = True
  MulIns == MulIns = True
  Ignore == Ignore = True
  Do == Do = True
  DoNot == DoNot = True
  _ == _ = False

TokenKind ProgKind where
  TokType LParen = ()
  TokType RParen = ()
  TokType Comma = ()
  TokType MulIns = ()
  TokType Ignore = ()
  TokType Do = Bool
  TokType DoNot = Bool
  TokType Number = Integer

  tokValue LParen _ = ()
  tokValue RParen _ = ()
  tokValue Comma _ = ()
  tokValue MulIns _ = ()
  tokValue Ignore _ = ()
  tokValue Do _ = True
  tokValue DoNot _ = False
  tokValue Number x = cast x

ProgToken : Type
ProgToken = Token ProgKind

tmap : TokenMap ProgToken
tmap = [
  (is '(', Tok LParen),
  (is ')', Tok RParen),
  (is ',', Tok Comma),
  (exact "mul", Tok MulIns),
  (digits, Tok Number),
  (any, Tok Ignore)
  ]

mulInsGrammar : Grammar state ProgToken True (Integer, Integer)
mulInsGrammar = do match MulIns
                   match LParen
                   n1 <- match Number
                   match Comma
                   n2 <- match Number
                   match RParen
                   pure (n1, n2)

corruptGrammar : Grammar state ProgToken True ()
corruptGrammar = do _ <- match Number
                    pure ()
              <|> (match Ignore)
              <|> (match LParen)
              <|> (match RParen)
              <|> (match Comma)
              <|> (match MulIns)

codeGrammar : Grammar state ProgToken True ?td
codeGrammar = some (choose mulInsGrammar corruptGrammar)

sol1 : String -> Integer
sol1 = lex tmap
     ||> fst
     ||> parse codeGrammar
     ||> getRight
     ||> map (fst
             ||> forget
             ||> lefts
             ||> (map (uncurry (*)))
             ||> sum
             )
     ||> fromMaybe 0

tmap' : TokenMap ProgToken
tmap' = [
  (exact "do()", Tok Do),
  (exact "don't()", Tok DoNot),
  (is '(', Tok LParen),
  (is ')', Tok RParen),
  (is ',', Tok Comma),
  (exact "mul", Tok MulIns),
  (digits, Tok Number),
  (any, Tok Ignore)
  ]

enableGrammar : Grammar state ProgToken True Bool
enableGrammar = (match Do) <|> (match DoNot)

codeGrammar' : Grammar state ProgToken True ?ty
codeGrammar' = some $ choose ( choose enableGrammar mulInsGrammar ) corruptGrammar

sol2 : String -> ?sol2ty
sol2 = lex tmap'
     ||> fst
     ||> parse codeGrammar'
     ||> getRight
     ||> map (fst
             ||> forget
             ||> lefts
             ||> foldl fn (True, 0)
             ||> snd
             )
     ||> fromMaybe 0
     where fn : (Bool, Integer) -> Either Bool (Integer, Integer) -> (Bool, Integer)
           fn (True, sum)  ( Right (x, y) ) = (True, sum + x * y)
           fn (False, sum) ( Right _ )      = (False, sum)
           fn (_, sum)     (Left b)         = (b, sum)


ex1 : String
ex1 = """
xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))
"""

ex2 : String
ex2 = """
xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))
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
