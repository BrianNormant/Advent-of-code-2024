module Lib.Maybe

import Data.Maybe
import Data.These

%default total


export
pairMaybe : (Maybe a, Maybe b) -> Maybe (a, b)
pairMaybe (x, y) = [| (x, y) |]

export
pairMaybe' : Maybe a -> Maybe b -> Maybe (a, b)
pairMaybe' = curry pairMaybe

export
theseMaybe : (Maybe a, Maybe b) -> Maybe (These a b)
theseMaybe (Just x, Just y) = Just $ Both x y
theseMaybe (Just x, Nothing) = Just $ This x
theseMaybe (Nothing, Just y) = Just $ That y
theseMaybe (Nothing, Nothing) = Nothing
