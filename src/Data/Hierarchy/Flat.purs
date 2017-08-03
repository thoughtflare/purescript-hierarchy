module Data.Hierarchy.Flat (mkFlat)
       where

import Data.Foldable (foldl)
import Data.Hierarchy (Hierarchy(..))
import Data.Maybe (Maybe(..))
import Prelude (class Eq, flip, otherwise, (==))

mkFlat :: forall a. Eq a => a -> Hierarchy a
mkFlat = flip Cont []

{--
at :: forall a. Eq a => Hierarchy a -> a -> Maybe (Hierarchy a)
at h x = foldl getDo Nothing h
  where
    getDo r@(Just _) _ = r
    getDo Nothing e
      | x == e    = Just e
      | otherwise = Nothing
--}
