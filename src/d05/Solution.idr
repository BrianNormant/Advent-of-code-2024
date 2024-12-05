module Main

import Text.Lexer
import Text.Parser

import Data.Integral
import Data.String
import Data.List
import Data.Maybe
import Data.Either
import Debug.Trace
import Data.Fin

import System.File
import Lib

%default total

FILENAME : String
FILENAME = "./inputs/d05.txt"

data RuleKind = Number
              | SepVer
              | Comma
              | SepLin

Eq RuleKind where
  Number == Number = True
  SepVer == SepVer = True
  Comma == Comma = True
  SepLin == SepLin = True
  _ == _ = False

TokenKind RuleKind where
  TokType Number = Integer
  TokType SepVer = ()
  TokType Comma = ()
  TokType SepLin = ()

  tokValue Number x = cast x
  tokValue SepVer _ = ()
  tokValue Comma _ = ()
  tokValue SepLin _ = ()

RuleToken : Type
RuleToken = Token RuleKind

tmap : TokenMap RuleToken
tmap = [
  (digits, Tok Number),
  (is '|', Tok SepVer),
  (is ',', Tok Comma),
  (newline, Tok SepLin)
  ]

ruleGrammar : Grammar state RuleToken True (Integer, Integer)
ruleGrammar = do n1 <- match Number
                 match SepVer
                 n2 <- match Number
                 pure(n1,n2)

updateGrammar : Grammar state RuleToken True ( List1 Integer )
updateGrammar = sepBy1 (match Comma) (match Number)

orderingGrammar : Grammar state RuleToken True (List1 (Integer,Integer), List1 (List1 Integer))
orderingGrammar = do p1 <- sepEndBy1 (match SepLin) ruleGrammar
                     match SepLin
                     p2 <- sepEndBy1 (match SepLin) updateGrammar
                     pure (p1, p2)

isRespectingRuleSet : List (Integer, Integer) -> List Integer -> Bool
isRespectingRuleSet [] _ = True
isRespectingRuleSet ((b,a)::rs) c =
  let pb = finToNat <$> findIndex (== b) c
      pa = finToNat <$> findIndex (== a) c
      m = (,) <$> pb <*> pa
      r = delay $ (isRespectingRuleSet rs c)
   in maybe r (tmp r) m
   where -- seems to be a bug in the compiler because this need to be extracted
         -- for the typecheck to succedd. Maybe it just needs to be more explicit?
   tmp : Bool -> (Nat, Nat) -> Bool
   tmp r (i, j) = (i > j) <|> (False, r)

sol1 : String -> ?sol1ty
sol1 = lex tmap
   ||> fst
   ||> parse orderingGrammar
   ||> getRight
   ||> map (fst
        ||> bimap forget (forget ||> map forget)
        ||> apply (\(ruleset, candidates) =>
                  filter (
                    isRespectingRuleSet ruleset
                    ) candidates
                  )
        ||> map middle
        ||> sequence
           )
   ||> join
   ||> map sum
   ||> fromMaybe 0

RuleSet : Type
RuleSet = List (Integer, Integer)

Candidates : Type
Candidates = List (List Integer)

--- list, (relation a b)
--- if a et b in list
--- if idx a > idx b
--- move a before b
--- [ ..., b, ..., a, ...]
--- [...] [b] [...] [a] [...]
--- [...] [a, b] [...] [...]
--- invalid [...] [a] [...] [b] [...]
--- continue with the rest of the rules
reorder : RuleSet -> List Integer -> List Integer
reorder [] l = l
reorder ((a, b)::rs) l =
  let idxa = findIndex (== a) l
      idxb = findIndex (== b) l
      m = (,) <$> idxa <*> idxb
   in reorder rs $ (maybe l (tmp l) m)
  where
    tmp : ( l : List Integer) -> (Fin (length l), Fin (length l)) -> List Integer
    tmp l (a, b) = (a > b) <|> (swapAt' l a b, l)

partial
sol2 : String -> ?sol2ty
sol2 = lex tmap
   ||> fst
   ||> parse orderingGrammar
   ||> getRight
   ||> map (Builtin.fst
        ||> bimap forget (forget ||> map forget)
        ||> process
           )
   ||> fromMaybe 0
   where
     process : (RuleSet, Candidates) -> Integer
     process (ruleset, candidates) =
                  filter ( not . isRespectingRuleSet ruleset) candidates
                  |> map (untilSame (reorder ruleset))
                  |> map middle
                  |> traverse id
                  |> maybe 0 sum


ex1 : String
ex1 = """
47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47
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
