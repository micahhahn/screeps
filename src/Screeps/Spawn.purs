module Screeps.Spawn
    ( BodyType(..)
    , ErrCode(..)
    , spawnCreep
    , getSpawns
    , gameNotify
    , getRawMemory
    , setRawMemory
    ) where
  
import Data.Either
import Data.Function.Uncurried
import Data.Maybe
import Effect.Exception
import Prelude
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

import Effect (Effect)

data Spawn

data BodyType = BodyWork
              | BodyMove
              | BodyCarry
              | BodyAttack
              | BodyRangedAttack
              | BodyHeal
              | BodyTough
              | BodyClaim

encodeBodyType :: BodyType -> String
encodeBodyType BodyWork = "work"
encodeBodyType BodyMove = "move"
encodeBodyType BodyCarry = "carry"
encodeBodyType BodyAttack = "attack"
encodeBodyType BodyRangedAttack = "ranged_attack"
encodeBodyType BodyTough = "tough"
encodeBodyType BodyHeal = "heal"
encodeBodyType BodyClaim = "claim"

data ErrCode = ErrNotOwner
             | ErrNoPath
             | ErrNameExists
             | ErrBusy
             | ErrNotFound
             | ErrNotEnoughEnergy
             | ErrNotEnoughResourcesOrExtensions
             | ErrInvalidTarget
             | ErrFull
             | ErrNotInRange
             | ErrInvalidArgs
             | ErrTired
             | ErrNoBodypart
             | ErrRclNotEnough
             | ErrGclNotEnough
             
derive instance genericErrCode :: Generic ErrCode _

instance showErrCode :: Show ErrCode where
  show = genericShow

parseErrCode :: Int -> Maybe ErrCode
parseErrCode (-1) = Just ErrNotOwner
parseErrCode (-2) = Just ErrNoPath
parseErrCode (-3) = Just ErrNameExists
parseErrCode (-4) = Just ErrBusy
parseErrCode (-5) = Just ErrNotFound
parseErrCode (-6) = Just ErrNotEnoughResourcesOrExtensions
parseErrCode (-7) = Just ErrInvalidTarget
parseErrCode (-8) = Just ErrFull
parseErrCode (-9) = Just ErrNotInRange
parseErrCode (-10) = Just ErrInvalidArgs
parseErrCode (-11) = Just ErrTired
parseErrCode (-12) = Just ErrNoBodypart
parseErrCode (-14) = Just ErrRclNotEnough
parseErrCode (-15) = Just ErrGclNotEnough
parseErrCode _ = Nothing

type RoomPosition = { roomName :: String, x :: Number, y :: Number }

type GameCpu = { limit :: Number, tickLimit :: Number, bucket :: Number, unlocked :: Boolean, unlockedTime :: Number }

type Game = { cpu :: GameCpu }

type SpawnCreepOpts = { directions :: Array String, dryRun :: Boolean }

foreign import spawnCreep_ :: Fn4 String (Array String) String SpawnCreepOpts (Effect Int)

spawnCreep :: String -> Array BodyType -> String -> Effect (Either Unit ErrCode)
spawnCreep spawnId bodyTypes spawnName = do
    code <- runFn4 spawnCreep_ spawnId (map encodeBodyType bodyTypes) spawnName { directions: [], dryRun: false }
    if code == 0 then pure (Left unit)
                 else case parseErrCode code of
                      Just e -> pure (Right e)
                      Nothing -> throw ("Unrecognized return code in call to \"spawnCreep\": " <> show code)

foreign import spawns_ :: Effect (Array String)

getSpawns :: Effect (Array String)
getSpawns = spawns_

foreign import gameNotify_ :: Fn2 String Number (Effect Unit)

gameNotify :: String -> Number -> Effect Unit
gameNotify = runFn2 gameNotify_

foreign import getRawMemory_ :: Effect String
foreign import setRawMemory_ :: Fn1 String (Effect Unit)

getRawMemory :: Effect String
getRawMemory = getRawMemory_

setRawMemory :: String -> Effect Unit
setRawMemory = runFn1 setRawMemory_