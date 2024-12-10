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

export
||| get the element valid from a predicate
||| starting at the end of the seq
lastThat : (a -> Bool) -> Seq a -> Maybe a
lastThat p s with (last s)
  _ | Nothing = Nothing
  _ | Just x with (p x)
    _ | True = Just x
    _ | False = assert_total lastThat p (init s)


export
||| split the seq at the first element valid from a predicate
||| starting at the front of the seq
||| ex: splitFirstWhere (== 0) [1,2,0,4,5] = ([1,2], [0, 4, 5])
splitFirstWhere : (a -> Bool) -> Seq a -> (Seq a, Seq a)
splitFirstWhere p s with (head s)
  _ | Nothing = (empty, empty)
  _ | Just x with (p x)
    _ | True = (empty, s)
    _ | False = let (a,b) = assert_total splitFirstWhere p (tail s)
                 in (cons x a, b)

export
dropUntil : (a -> Bool) -> Seq a -> Seq a
dropUntil p s with (head s)
  _ | Nothing = empty
  _ | Just x with (p x)
    _ | True = s
    _ | False = assert_total dropUntil p (tail s)
