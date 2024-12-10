module Silly

import Lib
import Data.SortedMap
import Debug.Trace

Memory : Type
Memory = SortedMap Integer Integer


||| memoized version of fibonacci sequence
fib : Integer -> Integer
fib = fib' empty ||> snd
    where fib' : Memory -> Integer -> (Memory, Integer)
          fib' m 0 = (m, 0) |> traceVal
          fib' m 1 = (m, 1) |> traceVal
          fib' m k with (lookup k m)
            _ | Just v = (m, v)
            _ | Nothing = let (m, n1) = fib' m (k - 1)
                              --m = insert (k - 1) n1 m
                              (m, n2) = fib' m (k - 2)
                              --m = insert (k - 1) n1 m
                              r = n1 + n2
                              m = insert k r m
                           in (m, r)

