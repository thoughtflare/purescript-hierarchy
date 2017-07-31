module Data.Hierarchy.Flat (mkFlat)
       where

import Data.Hierarchy (Hierarchy(..))
import Data.Maybe (Maybe(..))
import Prelude (class Eq, flip)

mkFlat :: forall a. Eq a => a -> Hierarchy a
mkFlat = flip Cont []

getAt :: forall a. Eq a => Hierarchy a -> a -> Maybe (Hierarchy a)
getAt h _ = Just h
