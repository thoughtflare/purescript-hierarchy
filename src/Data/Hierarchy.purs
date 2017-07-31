module Data.Hierarchy (Hierarchy(..)) where

import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Generic (gDecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Generic (gEncodeJson)
import Data.Generic (class Generic, gShow)
import Prelude (class Functor, class Show, map, ($))

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
