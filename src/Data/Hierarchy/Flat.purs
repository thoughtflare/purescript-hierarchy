module Data.Hierarchy.Flat (mkFlat, at, insert, insertv)
       where

import Data.Generic (class Generic)
import Data.Hierarchy (Hierarchy(..), foldTill)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst, snd)
import Prelude (class Eq, append, flip, map, otherwise, unit, ($), (/=), (==))

mkFlat :: forall a. Eq a => a -> Hierarchy a
mkFlat = flip Cont []

at :: forall a. Eq a => Hierarchy a -> a -> Maybe (Hierarchy a)
at h@(Cont x xs) v = snd $ foldTill g (Tuple true unit) h
  where
    g _ v1
      | v1 == v   = Tuple false unit
      | otherwise = Tuple true  unit

insertv :: forall a. Generic a => Eq a => Hierarchy a -> a -> a -> Maybe (Hierarchy a)
insertv h x y = insert h x [(mkFlat y)]

insert :: forall a. Generic a => Eq a => Hierarchy a -> a -> Array (Hierarchy a) -> Maybe (Hierarchy a)
insert h x hs = let
  ok = insertDo h
  in
   if h /= ok then Just ok else Nothing
  where
    insertDo h1@(Cont _ []) = h1
    insertDo h1@(Cont v vs)
      | v == x    = Cont v (vs `append` hs)
      | otherwise = Cont v (map insertDo vs)
