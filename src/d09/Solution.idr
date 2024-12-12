-- module Main

import Data.List
import Data.List.Extra
import Data.Maybe
import Data.Nat
-- import Data.Seq.Unsized
-- import Data.Seq.Internal
import Data.Queue
import Data.String

import Debug.Trace

import System.File

import Lib

%default total

FILENAME : String
FILENAME = "./inputs/d09.txt"

record Block where
  constructor MkBlock
  id : Maybe Int

isEmpty : Block -> Bool
isEmpty = id ||> isNothing

Show Block where
  show (MkBlock (Just i)) = cast (i)
  show (MkBlock Nothing) = "."

DList : Type -> Type
DList = Queue

parse1 : String -> DList Block
parse1 = unpack
     ||> map (\c => (the Int $ cast c) - 48 )
     ||> grouped 2
     ||> mapi (\i,l => case l of
              [b, e] => replicate (cast b) (MkBlock (Just $ (cast i)))
                     ++ replicate (cast e) (MkBlock Nothing)
              [b]    => replicate (cast b) (MkBlock (Just $ (cast i)))
              _      => []
              )
     ||> join
     ||> fromList
     ||> balance

debug : DList Block -> String
debug = map show ||> toList ||> joinBy ""


defrag : DList Block -> (DList Block, DList Block)
defrag s with (any (isEmpty) s)
  _ | False = (s, empty)
  _ | True with (last s)
    _ | Just x = let (r, s) = swapFistIf isEmpty x s
                           |> init
                           |> splitFirstWhere isEmpty
                     (r', s') = assert_total defrag s
                  in (r ++ r', s')
    _ | Nothing = (empty, empty)

checksum : DList Block -> Nat
checksum = toList
       ||> map (\b => id b)
       ||> catMaybes
       ||> map cast
       ||> mapi (\i,b => i * b)
       ||> sum

sol1 : String -> ?sol1ty
sol1 = parse1 ||> defrag ||> fst ||> checksum

parse2 : String -> DList (Int, Int)
parse2 = unpack
     ||> map (\c => (the Int $ cast c) - 48 )
     ||> grouped 2
     ||> mapi (\i,l => case l of
              [b, e] => [((cast i)+1, b),
                         (0, e)]
              [b]    => [((cast i)+1, b)]
              _      => []
              )
     ||> join
     ||> filter (snd ||> (/= 0))
     ||> fromList

debug2 : DList (Int, Int) -> String
debug2 = toList
     ||> map (\(i,s) => case i of
                             0 => replicate (cast s) '.'
                             _ => replicate (cast s) (cast (i + 47))
             )
     ||> joinBy ""

||| Swap if the block can fit
||| when swapping, split the empty space
||| to have the block not modify the replaced space
customSwapIf : (Int, Int) -> DList (Int, Int) -> (Bool, DList (Int, Int))
customSwapIf (id, si) s with (head s)
  _ | Nothing = (False, empty)
  _ | Just x with ((fst x) == 0 && id /= 0 && si <= (snd x))
    _ | False = let (r, t) = assert_total customSwapIf (id, si) (tail s)
                 in (r, cons x t)
    _ | True = if (si == snd x)
               then (True, cons (id, si) (tail s))
               else (True, cons (id, si) (
                    cons (0, (snd x) - si) (tail s)
                    ))

total
defragStep : DList (Int, Int) -> DList (Int, Int)
-- defragStep s = s
defragStep s with (last s)
  _ | Just x = let (r, t) = customSwapIf x s
                   t = init t |> assert_total defragStep
                in r <|> (
                  snoc t (0, snd x),
                  snoc t x
                  )
  _ | Nothing = empty

checksum' : List Int -> Int
checksum' = mapi (\i,id => if id > 0 then (cast i) * id else 0)
        ||> sum

partial
sol2 : String -> ?sol2ty
sol2 = parse2
   ||> defragStep
   -- ||> debug2
   ||> toList
   -- ||> filter (fst ||> (/= 0))
   ||> map (\(i,s) => the (List Int) $ List.replicate (cast s) (i - 1))
   ||> join
   ||> checksum'

ex1 : String
ex1 = """
12345
"""

ex2 : String
ex2 = """
2333133121414131402
"""

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

public export partial
main : IO ()
main = run1
