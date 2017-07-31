module Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Functor (map)
import Data.Hierarchy (Hierarchy(..))
import Data.String (length)
import Prelude (Unit, show, ($), (<<<))

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  (log <<< show <<< map length) $ Cont "Hello" [Cont "world!" []]
