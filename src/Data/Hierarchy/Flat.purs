module Data.Hierarchy.Flat (mkFlat)
       where

import Data.Hierarchy (Hierarchy(..))
import Prelude (class Eq, flip)

mkFlat :: forall a. Eq a => a -> Hierarchy a
mkFlat = flip Cont []
