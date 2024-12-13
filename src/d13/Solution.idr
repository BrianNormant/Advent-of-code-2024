module Main

import Data.List
import Data.Maybe
import Data.Either
import Data.String

import Debug.Trace

import Text.Lexer
import Text.Parser

import System.File
import Derive.Prelude

import Text.PrettyPrint.Bernardy

import Lib

%language ElabReflection
%default total

FILENAME : String
FILENAME = "./inputs/d13.txt"

data ClawKind = BtnA
              | BtnB
              | Prz
              | SepCom
              | SepSpc
              | SepLin
              | XLit
              | YLit
              | PLit
              | ELit
              | Number

%runElab derive "ClawKind" [Eq]

TokenKind ClawKind where
  TokType BtnA = ()
  TokType BtnB = ()
  TokType Prz = ()
  TokType SepCom = ()
  TokType SepSpc = ()
  TokType SepLin = ()
  TokType XLit = ()
  TokType YLit = ()
  TokType PLit = ()
  TokType ELit = ()
  TokType Number = Integer

  tokValue BtnA _ = ()
  tokValue BtnB _ = ()
  tokValue Prz _ = ()
  tokValue SepCom _ = ()
  tokValue SepSpc _ = ()
  tokValue SepLin _ = ()
  tokValue XLit _ = ()
  tokValue YLit _ = ()
  tokValue PLit _ = ()
  tokValue ELit _ = ()
  tokValue Number x = cast x

ClawToken : Type
ClawToken = Token ClawKind

tmap : TokenMap ClawToken
tmap = [
  (exact "Button A:", Tok BtnA),
  (exact "Button B:", Tok BtnB),
  (exact "Prize:", Tok Prz),
  (is ',', Tok SepCom),
  (newline, Tok SepLin),
  (spaces, Tok SepSpc),
  (is 'X', Tok XLit),
  (is 'Y', Tok YLit),
  (is '+', Tok PLit),
  (is '=', Tok ELit),
  (digits, Tok Number)
  ]

linAGrammar : Grammar state ClawToken True (Integer, Integer)
linAGrammar = do match BtnA
                 match SepSpc
                 match XLit
                 match PLit
                 n <- match Number
                 match SepCom
                 match SepSpc
                 match YLit
                 match PLit
                 m <- match Number
                 match SepLin
                 pure (n, m)

linBGrammar : Grammar state ClawToken True (Integer, Integer)
linBGrammar = do match BtnB
                 match SepSpc
                 match XLit
                 match PLit
                 n <- match Number
                 match SepCom
                 match SepSpc
                 match YLit
                 match PLit
                 m <- match Number
                 match SepLin
                 pure (n, m)

linPGrammar : Grammar state ClawToken True (Integer, Integer)
linPGrammar = do match Prz
                 match SepSpc
                 match XLit
                 match ELit
                 x <- match Number
                 match SepCom
                 match SepSpc
                 match YLit
                 match ELit
                 y <- match Number
                 match SepLin
                 pure (x, y)

Machine : Type
Machine = ((Integer, Integer),(Integer, Integer),(Integer, Integer))

MachineD : Type
MachineD = ((Double, Double),(Double, Double),(Double, Double))

machineGrammar : Grammar state ClawToken True Machine
machineGrammar = do a <- linAGrammar
                    b <- linBGrammar
                    p <- linPGrammar
                    pure (a, b, p)

pureGrammar : Grammar state ClawToken True (List1 Machine)
pureGrammar = sepEndBy1 (match SepLin) machineGrammar

toMachineD : Machine -> MachineD
toMachineD ((ax, ay), (bx, by), (x, y)) = (
  (cast ax, cast ay),
  (cast bx, cast by),
  (cast x, cast y)
  )


round : Double -> Double
round x = let f = floor x
              d = x - f
           in if d < 0.5 then f else f + 1.0

roundN : Double -> Nat
roundN x = cast $ round x

isInt : Double -> Bool
isInt x = abs (x - round x) < 0.001

solveMachine : Machine -> (Double, Double)
solveMachine m =
  let ((ax, ay), (bx, by), (x, y)) = toMachineD m
      n = (y - ((x * by) / bx)) / (ay - ((ax * by) / bx))
      n' = (x - (n * ax)) / bx
   in (n, n')

sol1 : String -> ?sol1ty
sol1 = lex tmap
   ||> fst
   ||> parse pureGrammar
   ||> getRight
   ||> map (fst
           ||> forget
           ||> map solveMachine
           ||> filter (bimap isInt isInt ||> (\(a, b) => a && b))
           ||> map (bimap round round)
           ||> filter (\(a, b) => a <= 100 && b <= 100)
           ||> map ( \(a, b) => a * 3 + b )
           ||> sum
           )

decimal : Double -> Double
decimal x = x - floor x

sol2 : String -> ?sol2ty
sol2 = lex tmap
   ||> fst
   ||> parse pureGrammar
   ||> getRight
   ||> map (fst
           ||> forget
           ||> map (\(x,y,(px,py)) => (x, y, (px + 10000000000000, py + 10000000000000)))
           ||> map solveMachine
           ||> filter (\(a,b) => a >= 0 && b >= 0)
           -- ||> map (\(a,b) => (decimal a, decimal b, isInt a && isInt b))
           ||> filter (bimap isInt isInt ||> (\(a, b) => a && b))
           ||> map (bimap roundN roundN)
           ||> map ( \(a, b) => a * 3 + b )
           ||> sum
           )

ex1 : String
ex1 = """
Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279

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
               Right line => putStrLn $ Doc.render opts $ pretty $ sol1 line
               Left _ => putStrLn "Error reading file"

export
partial
run2 : IO()
-- run2 = putStrLn $ Doc.render opts $ pretty $ sol2 ex2
run2 = do file <- readFile FILENAME
          case file of
               Right line => putStrLn $ Doc.render opts $ pretty $ sol2 line
               Left _ => putStrLn "Error reading file"
