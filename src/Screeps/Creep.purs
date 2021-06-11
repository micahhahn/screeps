module Screeps.Creep where

import Data.Function.Uncurried
import Data.Newtype
import Effect
import Foreign
import Prelude
import Record
import Screeps.Types

foreign import attack_ :: Fn2 Foreign Foreign Int

attackCreep :: Creep -> Creep -> Effect Unit
attackCreep (Creep source) (Creep target) = do
    let result = runFn2 attack_ source.handle target.handle
    pure unit
