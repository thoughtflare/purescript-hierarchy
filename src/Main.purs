module Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Prelude (Unit, show, ($), (<<<))

import Data.Hierarchy (Hierarchy(..))

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  (log <<< show) $ Cont "Hello" [Cont "world" []]
