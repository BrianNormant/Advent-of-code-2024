module Lib.Queue

import Data.Queue
import Data.List
import Data.SnocList

import Lib.SnocList

%default total

export
fromList : List a -> SnocList a
fromList = ( fromList' Lin )
        where fromList' : SnocList a -> List a -> SnocList a
              fromList' acc [] = acc
              fromList' acc (x::xs) = fromList' (acc :< x) xs


export
||| take the first value of a queue
head : Queue a -> Maybe a
head q = map fst (dequeue q)

export
||| take all the values after the first value of a queue
tail : Queue a -> Queue a
tail q with (dequeue q)
  _ | Just (_, xs) = xs
  _ | Nothing = empty

export
||| take the last value of a queue
last : Queue a -> Maybe a
last (Q front back) with (back)
  _ | Lin = List.last' front
  _ | (_ :< x) = Just x

export
||| take all the values before the last value of a queue
init : Queue a -> Queue a
init (Q [] [<]) = (Q [] [<])
init (Q [] (xs:<_)) = (Q [] xs)
init (Q xs [<]) = init' xs |> maybe empty (\x => (Q x [<]))
init (Q xs (ys:<_)) = (Q xs ys)

export
||| reorder the queue such that it's internal representation of
||| a list and a snoclist have a balanced number of element
balance : Queue a -> Queue a
balance (Q [] [<]) = Q [] [<]
balance (Q xs [<]) = let n = cast $ div (cast $ length xs) 2
                         (fst, lst) = splitAt n xs
                         lst = SnocList.fromList lst
                      in Q fst lst
balance (Q [] xs) = let n = cast $ div (cast $ length xs) 2
                        (fst, lst) = splitAt n xs
                        fst = toList fst
                     in Q fst lst
balance q@(Q fst lst) =
  let n = cast $ div (cast $ length q) 2
      all = fst ++ toList lst
      (fst, lst) = splitAt n all
      lst = SnocList.fromList lst
   in Q fst lst


export
||| prepend a value to a queue
cons : a -> Queue a -> Queue a
cons = prepend


export
||| append a value to a queue
snoc : Queue a -> a -> Queue a
snoc = enqueue

export
||| swap the first element according to a predicate
swapFistIf : (a -> Bool) -> a -> Queue a -> Queue a
swapFistIf p e q with (head q)
  _ | Nothing = empty
  _ | Just x = if (p x)
                  then assert_total $ cons e (tail q)
                  else assert_total $ cons x (swapFistIf p e (tail q))

export
splitFirstWhere : (a -> Bool) -> Queue a -> (Queue a, Queue a)
splitFirstWhere p q with (head q)
  _ | Nothing = (empty, empty)
  _ | Just x = if p x
                  then (empty, q)
                  else (let (a, b) = assert_total splitFirstWhere p (tail q)
                         in (cons x a, b))
