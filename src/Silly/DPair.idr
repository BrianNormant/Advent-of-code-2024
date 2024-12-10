module Silly.DPair

import Data.String
import Data.Vect

||| Interestingly we can never `forget` about
||| the dependency between n and Vect n a
||| as doing so at any point in the program
||| would be akin to letting the caller
||| choose the value of n.
||| example:
||| ```
||| fn : ... -> Vect n a
||| fn = s getLine -- would be wrapped in a IO
|||   |> fromText f
|||   -- some processing
|||   |> the (Vect 4 Int) -- During unification, the
|||                       -- constraint here will force
|||                       -- the type of Vect to
|||                       -- be 4
||| ```
fromText : (String -> a) -> String -> (n ** Vect n a)
fromText f s = let r = lines s
                    |> map f
                   v = fromList r
                in (_ ** v)

||| This is an example with multidimentional vector dependent on the same size
fromText2 : (String -> List a) -> String -> (n ** Vect n (Vect n a))
fromText2 f s = let r = lines s
                     |> map f
                    v = map (toVect (length r)) r
                     |> catMaybes
                    v = toVect (length r) v
                 in maybe (_ ** []) (\v' => (_ ** v')) v
