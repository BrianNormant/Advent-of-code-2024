module Lib.SnocList

import Data.SnocList

%default total

export
splitAt : (n : Nat) -> (xs : SnocList a) -> (SnocList a, SnocList a)
splitAt Z xs = (xs, [<])
splitAt (S k) [<] = ([<], [<])
splitAt (S k) (xs :< x) =
  let (dr, tk) = splitAt k xs in
      (dr, tk :< x)

export
fromList : List a -> SnocList a
fromList = ( fromList' Lin )
        where fromList' : SnocList a -> List a -> SnocList a
              fromList' acc [] = acc
              fromList' acc (x::xs) = fromList' (acc :< x) xs
