module Lib.Vect

import Data.Vect

%default total

||| Remove the last element of a Vect
export
dropLast : Vect (S n) a -> Vect n a
dropLast = reverse . tail . reverse

export
indexMaybe : Nat -> Vect n a -> Maybe a
indexMaybe _ [] = Nothing
indexMaybe Z (x::_) = Just x
indexMaybe (S i) (x::xs) = indexMaybe i xs
