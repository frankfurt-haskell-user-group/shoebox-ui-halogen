module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Halogen.Aff (HalogenEffects, awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)

import GlossingPage (ui)

main :: Eff (HalogenEffects ()) Unit
main = runHalogenAff $ awaitBody >>= runUI ui unit

