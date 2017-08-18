module Data.Hierarchy.Flat ( mkFlat, at
                           , insert, insertv
                           , insert', insertv'
                           , update, updatev
                           , update', updatev'
                           , delete )
       where

import Data.Array (filter)
import Data.Foldable (find, foldl, length)
import Data.Functor ((<$>))
import Data.Generic (class Generic)
import Data.Hierarchy (Hierarchy(..), foldTill, toArray)
import Data.Map as M
import Data.Maybe (Maybe(..), isJust)
import Data.Ord (class Ord)
import Data.Tuple (Tuple(..), fst, snd)
import Prelude (class Eq, append, const, flip, map, otherwise, unit, ($), (+), (/=), (<), (<<<), (==))

mkFlat :: forall a. Eq a => a -> Hierarchy a
mkFlat = flip Cont []

at :: forall a. Eq a => Hierarchy a -> a -> Maybe (Hierarchy a)
at h@(Cont x xs) v = snd $ foldTill g (Tuple true unit) h
  where
    g _ v1
      | v1 == v   = Tuple false unit
      | otherwise = Tuple true  unit

isFlat :: forall a. Eq a => Ord a => Hierarchy a -> Boolean
isFlat h = isUnique $ toArray h
  where
    isUnique :: Array a -> Boolean
    isUnique xs = 0 == length (M.filter ((<) 1) (toMap xs))
    toMap :: Array a -> M.Map a Int
    toMap = foldl track M.empty
    track :: M.Map a Int -> a -> M.Map a Int
    track m x = trackDo m x (M.lookup x m)
    trackDo :: M.Map a Int -> a -> Maybe Int -> M.Map a Int
    trackDo m x (Just _) = M.update (\v -> Just $ v + 1) x m
    trackDo m x Nothing  = M.insert x 1 m

insert :: forall a. Generic a => Eq a => Ord a => Hierarchy a -> a -> Array (Hierarchy a) -> Hierarchy a
insert h x y = let
  ok = insertImpl h x y
  in
   if isFlat ok then ok else h

insertImpl :: forall a. Generic a => Eq a => Hierarchy a -> a -> Array (Hierarchy a) -> Hierarchy a
insertImpl h@(Cont _ []) _ _ = h
insertImpl h@(Cont v vs) x hs
  | v == x    = Cont v (vs `append` hs)
  | otherwise = Cont v (map insertDo vs)
  where
    insertDo h1 = insertImpl h1 x hs

insertv :: forall a. Generic a => Eq a => Ord a => Hierarchy a -> a -> a -> Hierarchy a
insertv h x y = insert h x [(mkFlat y)]

insertv' :: forall a. Generic a => Eq a => Ord a => Hierarchy a -> a -> a -> Maybe (Hierarchy a)
insertv' h x y = insert' h x [(mkFlat y)]

insert' :: forall a. Generic a => Eq a => Ord a => Hierarchy a -> a -> Array (Hierarchy a) -> Maybe (Hierarchy a)
insert' h x hs = let
  ok = insert h x hs
  in
   if h /= ok then Just ok else Nothing

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
