module Lib.Seq

import Data.Seq.Unsized

%default total

export
||| swap the first element according to a predicate
swapFistIf : (a -> Bool) -> a -> Seq a -> Seq a
swapFistIf p e s with (head s)
  _ | Nothing = empty
  _ | Just x  = case (p x) of
                     True => assert_total $ cons e (tail s)
                     False => assert_total $ cons x (swapFistIf p e (tail s))

export
||| swap the first element according to a predicate
||| Also return if an element was swapped
swapFistIfR : (a -> Bool) -> a -> Seq a -> (Bool, Seq a)
swapFistIfR p e s with (head s)
  _ | Nothing = (False, empty)
  _ | Just x  = case (p x) of
                     True => assert_total $ (True, cons e (tail s))
                     False => let (r, t) = assert_total swapFistIfR p e (tail s)
                              in (r, cons x t)


export
||| swap the last element according to a predicate
swapLastIf : (a -> Bool) -> a -> Seq a -> Seq a
swapLastIf p e s with (last s)
  _ | Nothing = empty
  _ | Just x with (p x)
    _ | True  = assert_total $ snoc (init s) e
    _ | False = assert_total $ snoc (swapLastIf p e $ init s) x
