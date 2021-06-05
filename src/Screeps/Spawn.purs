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

import Screeps.Internal.Constants

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
encodeBodyType BodyWork = kWORK
encodeBodyType BodyMove = kMOVE
encodeBodyType BodyCarry = kCARRY
encodeBodyType BodyAttack = kATTACK
encodeBodyType BodyRangedAttack = kRANGED_ATTACK
encodeBodyType BodyTough = kTOUGH
encodeBodyType BodyHeal = kHEAL
encodeBodyType BodyClaim = kCLAIM

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
parseErrCode i
  | i == kERR_NOT_OWNER = Just ErrNotOwner
  | i == kERR_NO_PATH = Just ErrNoPath
  | i == kERR_NAME_EXISTS = Just ErrNameExists
  | i == kERR_BUSY = Just ErrBusy
  | i == kERR_NOT_FOUND = Just ErrNotFound
  | i == kERR_NOT_ENOUGH_ENERGY = Just ErrNotEnoughEnergy
  | i == kERR_NOT_ENOUGH_RESOURCES = Just ErrNotEnoughResourcesOrExtensions
  | i == kERR_INVALID_TARGET = Just ErrInvalidTarget
  | i == kERR_FULL = Just ErrFull
  | i == kERR_NOT_IN_RANGE = Just ErrNotInRange
  | i == kERR_INVALID_ARGS = Just ErrInvalidArgs
  | i == kERR_TIRED = Just ErrTired
  | i == kERR_NO_BODYPART = Just ErrNoBodypart
  | i == kERR_NOT_ENOUGH_RESOURCES = Just ErrNotEnoughResourcesOrExtensions
  | i == kERR_RCL_NOT_ENOUGH = Just ErrRclNotEnough
  | i == kERR_GCL_NOT_ENOUGH = Just ErrGclNotEnough
  | otherwise = Nothing

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