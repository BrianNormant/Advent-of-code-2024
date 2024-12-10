module Silly.Async

import Data.List
import Data.List.Extra
import Data.Maybe
import IO.Async
import IO.Async.Loop.Epoll
import IO.Async.Internal.Token
import Debug.Trace

import Lib
--%enableReflexion

%default total

fibo : Nat -> Nat
fibo 0 = 0
fibo 1 = 1
fibo (S n@(S k)) = fibo n + fibo k

applyFn : (Nat, Nat) -> (Nat, Nat)
applyFn (i, n) = (i, fibo n) |> traceVal

||| parTraverse allows us to
||| do multiple, non-dependent computations
||| in parallel, and collect the results
||| for some obscure reason the syntax
||| (\n => lazy (operation n))
||| works whereas
||| (operation ||> lazy) or (lazy . operation)
||| doesn't, even tho they evaluate to the same thing
testSum : Async WorkST [] (Maybe (List (Nat, Nat)))
testSum = parTraverse (\n => lazy (applyFn n))
                      (replicate 1000 42 |> mapi MkPair)

compute : Async WorkST [] (Nat)
compute = do
  s <- testSum
  pure $ maybe 0 (map snd ||> sum) s

showMe : Async WorkST [] ()
showMe = compute
       >>= printLn

partial
main : IO ()
main = app 16 [] showMe

partial
main' : IO ()
main' = List.replicate 1000 42
     |> mapi MkPair
     |> map applyFn
     |> map fst
     |> sum
     |> printLn
