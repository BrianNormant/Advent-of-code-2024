module Main

import Data.List
import Data.Vect
import Data.Maybe
import Data.Either
import Data.String

import Derive.Prelude

import Debug.Trace

import System.File

import Text.PrettyPrint.Bernardy
import Text.Lexer
import Text.Parser

import Lib

%default total
%language ElabReflection

FILENAME : String
FILENAME = "./inputs/d14.txt"

data BathKind = Pos
              | Vel
              | Number
              | SepCom
              | SepSpc

%runElab derive "BathKind" [Eq]

TokenKind BathKind where
     TokType Pos = ()
     TokType Vel = ()
     TokType SepCom = ()
     TokType SepSpc = ()
     TokType Number = Int

     tokValue Pos _ = ()
     tokValue Vel _ = ()
     tokValue SepCom _ = ()
     tokValue SepSpc _ = ()
     tokValue Number x = cast x

BathToken : Type
BathToken = Token BathKind

tmap : TokenMap BathToken
tmap = [
     (exact "p=", Tok Pos),
     (exact "v=", Tok Vel),
     (is ',', Tok SepCom),
     (spaces, Tok SepSpc),
     (opt (is '-') <+> digits, Tok Number)
     ]

record Robot where
     constructor MkRobot
     pos : (Int, Int)
     vel : (Int, Int)

%runElab derive "Robot" [Show]

Pretty Robot where
     prettyPrec _ r = fromString $ show r

robotGrammar : Grammar e BathToken True Robot
robotGrammar = do match Pos
                  px <- match Number
                  match SepCom
                  py <- match Number
                  match SepSpc
                  match Vel
                  vx <- match Number
                  match SepCom
                  vy <- match Number
                  pure $ MkRobot (px, py) (vx, vy)

DIMX : Int
DIMX = 101
DIMY : Int
DIMY = 103

robotStep : Nat -> Robot -> Robot
robotStep Z r = r
robotStep (S k) r = robotStep k (step r) where
     step : Robot -> Robot
     step (MkRobot (x, y) (vx, vy)) =
          MkRobot (mod (x + vx) DIMX, mod (y + vy) DIMY)
                  (vx, vy)

Semigroup Nat where
     (<+>) = (+)

Monoid Nat where
     neutral = 0

count : List Robot -> Nat
count lr = foldl (\[q1, q2, q3, q4], r =>
                 let (MkRobot (x,y) _) = r
                  in if (x < (DIMX `div` 2))
                        then if (y < (DIMY `div` 2))
                                then [q1+1, q2, q3, q4]
                                else [q1, q2+1, q3, q4]
                        else if (y < (DIMY `div` 2))
                                then [q1, q2, q3+1, q4]
                                else [q1, q2, q3, q4+1]
                 ) (the (Vect 4 Nat) neutral ) lr
        |> foldl (*) 1

Semigroup Char where
     c <+> d = chr $ ord c + ord d
Monoid Char where
     neutral = '.'

robotGrid : List Robot -> Vect (cast DIMY) (Vect (cast DIMX) Char)
robotGrid lr = foldl (\v,(MkRobot (x,y) _) =>
               let x = tryFin (cast DIMX) (cast x)
                   y = tryFin (cast DIMY) (cast y)
                   p = pairMaybe (x,y)
                in maybe v (\c => replaceMat c v '#') p
     ) neutral lr

sol1 : String -> ?sol1ty
sol1 = lines
   ||> map (lex tmap
        ||> fst
        ||> parse robotGrammar
           )
   ||> rights
   ||> map fst
   ||> map (robotStep 100)
   ||> filter (\(MkRobot (x,y) _) => x /= (DIMX `div` 2) && y /= (DIMY `div` 2))
   ||> count
   -- ||> robotGrid
   -- ||> map (toList ||> pack)

sol2 : String -> ?sol2ty
sol2 _ = 2

ex1 : String
ex1 = """
p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3
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
