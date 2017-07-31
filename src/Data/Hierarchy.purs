module Data.Hierarchy (Hierarchy(..)) where

import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Generic (gDecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Generic (gEncodeJson)
import Data.Array (head, init, last, tail)
import Data.Foldable (class Foldable, foldMapDefaultL, foldl)
import Data.Generic (class Generic, gShow)
import Data.Maybe (Maybe(..))
import Prelude (class Functor, class Show, append, flip, map, ($))

data Hierarchy a = Cont a (Array (Hierarchy a))

derive instance genericHierarchy :: (Generic a) => Generic (Hierarchy a)

instance encodeJsonHierarchy :: (Generic a) => EncodeJson (Hierarchy a) where
  encodeJson = gEncodeJson
instance decodeJsonHierarchy :: (Generic a) => DecodeJson (Hierarchy a) where
  decodeJson = gDecodeJson
instance showHierarchy :: (Generic a, Show a) => Show (Hierarchy a) where
 show = gShow

instance functorHierarchy :: Functor Hierarchy where
  map g (Cont x []) = Cont (g x) []
  map g (Cont x xs) = Cont (g x) $ map (map g) xs

instance foldableHierarchy :: Foldable Hierarchy where
  foldl g b f@(Cont x xs) = foldDo g b f (head xs) (tail xs)
  foldr g b f@(Cont x xs) = foldDo (flip g) b f (last xs) (init xs)
  foldMap = foldMapDefaultL

foldDo :: forall a b .
           (b -> a -> b) ->
           b ->
           Hierarchy a ->
           Maybe (Hierarchy a) ->
           Maybe (Array (Hierarchy a)) ->
           b
foldDo g b (Cont x xs) Nothing _  = g b x
foldDo g b (Cont x xs) (Just (Cont x1 x1s)) (Just tl) =
  foldl g (g b x) (Cont x1 (x1s `append` tl))
foldDo _ b _ _ _ = b
