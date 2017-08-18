module Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Functor (map)
import Data.Hierarchy (Hierarchy(..))
import Data.Hierarchy.Flat (at, insert, delete, insertv)
import Data.Maybe (Maybe, fromJust)
import Data.String (length)
import Partial.Unsafe (unsafePartial)
import Prelude (Unit, show, ($), (<<<))

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
--  (log <<< show <<< map length) $ Cont "Hello" [Cont "world!" []]
--  (log <<< show) $ at (insert h "Mtg" [standard]) "Mtg"
  (log <<< show) $ insertv h "Misc" "Mtg"
--  (log <<< show) $ delete h "StarCraft"

fj :: forall a. Maybe a -> a
fj x = unsafePartial (fromJust x)

h :: Hierarchy String
h = Cont "Learning"
    [ Cont "StarCraft"
      [ Cont "Zerg" []
      , Cont "Terran" []
      , Cont "Protoss" [] ]
    , Cont "Mtg"
      [ Cont "Modern"
        [ Cont "DS" []
        , Cont "Affinity" [] ]
      , Cont "Limited"
        [ Cont "Sealed" []
        , Cont "Draft" [] ] ]
    , Cont "Misc" [] ]

standard :: Hierarchy String
standard = Cont "Standard"
           [ Cont "Mono Red" []
           , Cont "Zombies" [] ]
