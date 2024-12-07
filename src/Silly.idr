module Silly

import Data.Vect
import Data.List


filterVect : Vect n a -> (a -> Bool) -> (m ** Vect m a)
filterVect [] p = (0 ** [])
filterVect (x :: xs) p with (p x)
  _ | True = let (m ** rest) = filterVect xs p in ((S m) ** x::rest)
  _ | False = filterVect xs p
