module Data.Hierarchy.Flat ( mkFlat, at
                           , insert, insertv
                           , insert', insertv'
                           , update, updatev
                           , update', updatev'
                           , delete )
       where

import Data.Array (filter)
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

insertv' :: forall a. Generic a => Eq a => Hierarchy a -> a -> a -> Maybe (Hierarchy a)
insertv' h x y = insert' h x [(mkFlat y)]

insert' :: forall a. Generic a => Eq a => Hierarchy a -> a -> Array (Hierarchy a) -> Maybe (Hierarchy a)
insert' h x hs = let
  ok = insert h x hs
  in
   if h /= ok then Just ok else Nothing

insertv :: forall a. Generic a => Eq a => Hierarchy a -> a -> a -> Hierarchy a
insertv h x y = insert h x [(mkFlat y)]

insert :: forall a. Generic a => Eq a => Hierarchy a -> a -> Array (Hierarchy a) -> Hierarchy a
insert h@(Cont _ []) _ _ = h
insert h@(Cont v vs) x hs
  | v == x    = Cont v (vs `append` hs)
  | otherwise = Cont v (map insertDo vs)
  where
    insertDo h1 = insert h1 x hs

update :: forall a. Generic a => Eq a => Hierarchy a -> a -> Array (Hierarchy a) -> Hierarchy a
update h@(Cont _ []) _ _ = h
update h@(Cont v vs) x hs
  | v == x    = (Cont v hs)
  | otherwise = Cont v (map updateDo vs)
  where
    updateDo h1 = update h1 x hs

updatev :: forall a. Generic a => Eq a => Hierarchy a -> a -> a -> Hierarchy a
updatev h x y = update h x [(mkFlat y)]

update' :: forall a. Generic a => Eq a => Hierarchy a -> a -> Array (Hierarchy a) -> Maybe (Hierarchy a)
update' h x y = let
  ok = update h x y
  in
   if h /= ok then Just ok else Nothing

updatev' :: forall a. Generic a => Eq a => Hierarchy a -> a -> a -> Maybe (Hierarchy a)
updatev' h x y = update' h x [(mkFlat y)]

delete :: forall a. Generic a => Eq a => Hierarchy a -> a -> Hierarchy a
delete h@(Cont _ []) _ = h
delete h@(Cont v vs) x = Cont v $ map deleteDo (filter neq vs)
  where
    neq (Cont v vs)
      | v == x    = false
      | otherwise = true 
    deleteDo h1 = delete h1 x
