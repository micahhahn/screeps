module Screeps.Spawn
    ( BodyType(..)
    , ErrCode(..)
    , spawnCreep
    , getSpawns
    , gameNotify
    , getRawMemory
    , setRawMemory
    , gameSpawns
    ) where
  
import Control.Monad.Except
import Data.Argonaut
import Data.Either
import Data.Function.Uncurried
import Data.List.NonEmpty
import Data.Maybe
import Data.Traversable
import Data.Tuple
import Effect.Exception
import Foreign
import Prelude
import Screeps.Internal.Constants

import Control.Monad.ST.Internal (read)
import Data.Generic.Rep (class Generic)
import Data.Lazy (Lazy)
import Data.Map (Map)
import Data.Map as Map
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Foreign.Index as FI
import Foreign.Keys as FK
import Foreign.Object (Object)

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

foreign import getRawMemory_ :: Foreign
foreign import setRawMemory_ :: Fn1 String (Effect Unit)

foreign import gameSpawns_ :: Foreign

type ObjectEffect = { effect :: Number, level :: Number, ticksRemaining :: Number }

type RoomObject r = { effects :: Array ObjectEffect, pos :: RoomPosition | r}

type Structure2 = RoomObject ( hits :: Number, hitsMax :: Number, id :: String )

testX :: Structure2 -> Boolean
testX s = s.pos.x > 0.0

type Structure = { pos :: RoomPosition, hits :: Number, hitsMax :: Number, id :: String }

readStructureSpawn :: Foreign -> FT Effect Structure
readStructureSpawn v = do
  pos <- v FI.! "pos" >>= readRoomPosition
  hits <- v FI.! "hits" >>= readNumber
  hitsMax <- v FI.! "hitsMax" >>= readNumber
  id <- v FI.! "id" >>= readString
  pure { pos: pos, hits: hits, hitsMax: hitsMax, id: id }

readMap :: Foreign -> FT Effect (Map String Foreign)
readMap f = do
  ks <- FK.keys f
  vs' <- traverse (\i -> (Tuple i <$> FI.index f i)) ks
  pure (Map.fromFoldable vs')

gameSpawns :: Effect (Map String Structure)
gameSpawns = throwForeign $ traverse readStructureSpawn =<< readMap gameSpawns_

type RoomPosition = { roomName :: String, x :: Number, y :: Number }

readRoomPosition :: Foreign -> FT Effect RoomPosition
readRoomPosition v = do
  roomName <- v FI.! "roomName" >>= readString
  x <- v FI.! "x" >>= readNumber
  y <- v FI.! "y" >>= readNumber
  pure { roomName: roomName, x: x, y: y}

-- type Room = { controller :: Controller }
-- 
-- readRoom :: Foreign -> F Room
-- readRoom v = do
--   controller <- v ! "controller" >>= readStructureController
--   energyAvailable <- v ! "energyAvailable" >>= readInt
--   energyCapacityAvailable <- v ! "energyCapacityAvailable" >>= readInt
--   memory <- v ! "memory" >>= readMemory
--   name <- v ! "name" >>= readString
--   storage <- v ! "storage" >>= readStructureStorage
--   terminal <- v ! "terminal" >>= readStructureTerminal
--   visual <- v ! "visual" >>= readRoomVisual
--   pure { controller}


throwForeign :: forall a. ExceptT (NonEmptyList ForeignError) Effect a -> Effect a
throwForeign m = do
  ex <- runExceptT m
  case ex of
    Right v -> pure v
    Left e -> throw (foldl (<>) "" (map renderForeignError e))

getRawMemory :: Effect String
getRawMemory = throwForeign $ readString getRawMemory_

setRawMemory :: String -> Effect Unit
setRawMemory = runFn1 setRawMemory_

foreign import data RoomPosition_ :: Type
foreign import data CostMatrix :: Type

type Goal = { pos :: RoomPosition_, range :: Number }

foreign import mkRoomPosition_ :: Fn3 Number Number String RoomPosition_

type PathFinderOpts = { roomCallback :: Fn1 String CostMatrix, plainCost :: Number, swampCost :: Number, flee :: Boolean, maxOps :: Number, maxRooms :: Number, maxCost :: Number, heuristicWeight :: Number }

foreign import pathFinderSearch_ :: Fn3 RoomPosition_ (Array Goal) PathFinderOpts Json