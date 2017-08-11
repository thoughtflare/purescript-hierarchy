module Data.Hierarchy (Hierarchy(..), foldTill) where

import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Generic (gDecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Generic (gEncodeJson)
import Data.Array (head, init, last, tail)
import Data.Foldable (class Foldable, foldMapDefaultL)
import Data.Generic (class Generic, gShow)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst, snd)
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
  foldl g b f@(Cont x xs) = fst $ foldTill (constFst true g)
                                           (Tuple true b) f (head xs) (tail xs)
  foldr g b f@(Cont x xs) = fst $ foldTill (constFst true (flip g))
                                           (Tuple true b) f (last xs) (init xs)
  foldMap = foldMapDefaultL

constFst :: forall a b c . a -> (c -> b -> c) -> (Tuple a c -> b -> Tuple a c)
constFst x f y z = Tuple x (f (snd y) z)

foldTill :: forall a b .
           (Tuple Boolean b -> a -> Tuple Boolean b) ->
           Tuple Boolean b ->
           Hierarchy a ->
           Maybe (Hierarchy a) ->
           Maybe (Array (Hierarchy a)) ->
           Tuple b (Maybe (Hierarchy a))
foldTill _ (Tuple false acc) f _ _ = Tuple acc (Just f)
foldTill g b (Cont x xs) Nothing _ = Tuple (snd $ g b x) Nothing
foldTill g b (Cont x xs) (Just (Cont x1 x1s)) (Just tl) =
  foldTill g (g b x) (Cont x1 (x1s `append` tl)) (head (x1s `append` tl)) (tail (x1s `append` tl))
foldTill g b (Cont x xs) (Just (Cont x1 x1s)) Nothing =
  foldTill g (g b x) (Cont x1 x1s) (head x1s) (tail x1s)
