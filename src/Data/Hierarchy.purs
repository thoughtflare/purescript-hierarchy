module Data.Hierarchy (Hierarchy(..), foldTill) where

import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Generic (gDecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Generic (gEncodeJson)
import Data.Array (head, tail)
import Data.Foldable (class Foldable, foldMapDefaultL)
import Data.Generic (class Generic, gEq, gShow)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst, snd)
import Prelude (class Eq, class Functor, class Show, append, flip, map, ($))

-- | Hierarchy is a "general" tree. Leaves are encoded as
-- `Cont leafVal []`, branches have `Cont`s in the rightmost array.
-- Asymptotics of this data structure are pretty sad. Only use it for
-- small things such as hierarchical configurations.
data Hierarchy a = Cont a (Array (Hierarchy a))

-- | Working with `Tuple a c`, construct a function that makes a folding
-- step (`c -> b -> c`), taking initial value for `c` from the first tuple,
-- while keeping the first element of the Tuple (`a`) constant.
--
-- Drugs are bad, mmkay?
foldSnd :: forall a b c . a -> (c -> b -> c) -> (Tuple a c -> b -> Tuple a c)
foldSnd x f y z = Tuple x (f (snd y) z)


-- | A function that folds while the first element in the
-- accumulator tuple is `true`. When the first element in the
-- accumulator tuple becomes `false`, the function will return
-- a tuple with accumulator value and the remaining part of
-- the `Hierarchy` structure.
foldTill :: forall a b .
            (Tuple Boolean b -> a -> Tuple Boolean b) ->
            Tuple Boolean b ->
            Hierarchy a ->
            Tuple b (Maybe (Hierarchy a))
foldTill g b h@(Cont x xs) = foldTillDo g b h h (head xs) (tail xs)

foldTillDo :: forall a b .
           (Tuple Boolean b -> a -> Tuple Boolean b) ->
           Tuple Boolean b ->
           Hierarchy a ->
           Hierarchy a ->
           Maybe (Hierarchy a) ->
           Maybe (Array (Hierarchy a)) ->
           Tuple b (Maybe (Hierarchy a))

-- Return the acc if we don't need to continue
foldTillDo _ (Tuple false acc) _ b _ _ = Tuple acc (Just b)

-- If the head is empty, it means that we're looking at a leaf
-- so we update the accumulator and return it along with `Nothing`
-- showing that there is no more Hierarchy elements to parse.
foldTillDo g b (Cont x xs) _ Nothing _ = Tuple (snd $ g b x) Nothing

-- When we have both head and tail, we rotate the hierarchy, promoting
-- the head and sticking the tail to it. Then we recur.
foldTillDo g b h@(Cont x xs) _ (Just (Cont x1 x1s)) (Just tl) =
  foldTillDo g (g b x) (Cont x1 (x1s `append` tl)) h (head (x1s `append` tl)) (tail (x1s `append` tl))

-- Similar to the previous case, just digging into the tail of the
-- only child right away.
foldTillDo g b h@(Cont x xs) _ (Just (Cont x1 x1s)) Nothing =
  foldTillDo g (g b x) (Cont x1 x1s) h (head x1s) (tail x1s)

-- Some instances

derive instance genericHierarchy :: (Generic a) => Generic (Hierarchy a)

instance encodeJsonHierarchy :: (Generic a) => EncodeJson (Hierarchy a) where
  encodeJson = gEncodeJson
instance decodeJsonHierarchy :: (Generic a) => DecodeJson (Hierarchy a) where
  decodeJson = gDecodeJson

instance showHierarchy :: (Generic a, Show a) => Show (Hierarchy a) where
 show = gShow

instance eqHierarchy :: (Generic a, Eq a) => Eq (Hierarchy a) where
  eq = gEq

instance functorHierarchy :: Functor Hierarchy where
  map g (Cont x []) = Cont (g x) []
  map g (Cont x xs) = Cont (g x) $ map (map g) xs

instance foldableHierarchy :: Foldable Hierarchy where
  foldl g b f@(Cont x xs) = fst $ foldTill (foldSnd true g)        (Tuple true b) f
  foldr g b f@(Cont x xs) = fst $ foldTill (foldSnd true (flip g)) (Tuple true b) f
  foldMap = foldMapDefaultL
